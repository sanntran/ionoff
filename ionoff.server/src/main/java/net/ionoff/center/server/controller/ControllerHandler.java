package net.ionoff.center.server.controller;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;

import net.ionoff.center.server.service.IControlService;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.Switch;
import net.ionoff.center.server.message.RelayStatusNotifier;
import net.ionoff.center.server.message.SensorStatusNotifier;
import net.ionoff.center.server.message.event.RelayStatusChangedEvent;
import net.ionoff.center.server.message.event.SensorStatusChangedEvent;
import net.ionoff.center.server.persistence.dao.IControllerDao;
import net.ionoff.center.server.persistence.dao.ISensorDao;
import net.ionoff.center.server.persistence.dao.ISwitchDao;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.persistence.service.IControllerService;
import net.ionoff.center.server.persistence.service.IRelayService;
import net.ionoff.center.server.controller.exception.MessageFormatException;
import net.ionoff.center.server.controller.exception.ControllerRequestException;
import net.ionoff.center.server.controller.model.BaseStatus;
import net.ionoff.center.server.controller.model.EcIOStatus;
import net.ionoff.center.server.controller.model.EpIOStatus;
import net.ionoff.center.server.controller.model.ExIOStatus;
import net.ionoff.center.server.controller.model.PxIOStatus;
import org.springframework.stereotype.Component;

@Component
public class ControllerHandler {

	private static final Logger LOGGER = LoggerFactory.getLogger(ControllerHandler.class.getName());

	@Lazy
	@Autowired
	private ISensorDao sensorDao;

	@Lazy
	@Autowired
	private IRelayService relayService;

	@Lazy
	@Autowired
	private ISwitchDao switchDao;

	@Lazy
	@Autowired
	private IControllerService controllerService;
	
	@Lazy // very important to be lazy here due to cyclic dependency
	@Autowired
	private SensorStatusNotifier sensorStatusNotifier;
	
	@Lazy // very important to be lazy here due to cyclic dependency
	@Autowired
	private RelayStatusNotifier relayStatusNotifier;


	@Lazy
	@Autowired
	private IDeviceService deviceService;

	@Lazy
	@Autowired
	private IControllerDao controllerDao;

	@Lazy
	@Autowired
	IControlService controlService;

	public void onMessageArrived(String payload) {
		BaseStatus status = parseIOStatus(payload);
		if (status == null) {
			LOGGER.error("Unknown message format " + payload);
			return;
		}
		handleStatusMessage(status);
	}

	private BaseStatus parseIOStatus(String payload) {
		try {
			if (ExIOStatus.accept(payload)) {
				return new ExIOStatus(payload);
			} else if (PxIOStatus.accept(payload)) {
				return new PxIOStatus(payload);
			} else if (EcIOStatus.accept(payload)) {
				return new EcIOStatus(payload);
			} else if (EpIOStatus.accept(payload)) {
				return new EpIOStatus(payload);
			}
		} catch (Exception e) {
			LOGGER.error(e.getClass().getSimpleName() + " Error parsing message " + e.getMessage());
		}
		return null;
	}

	public void handleStatusMessage(BaseStatus status) {
		List<Controller> controllers = controllerDao.findByMac(status.getKey());
		if (controllers.isEmpty()) {
			LOGGER.info("No controller found. Key: " + status.getKey());
			return;
		}
		Controller controller = controllers.get(0);
		validateIOStatus(controller, status);
		if (!controller.isConnected()) {
			LOGGER.info("Controller " + controller.getKey() + " is now connected");
			if (controller.isLazy()) {
				controller.setConnectedTime(System.currentTimeMillis());
				controllerDao.update(controller);
				handleStarted(controller, status);
				return;
			}
		}
		controller.setConnectedTime(System.currentTimeMillis());
		controllerDao.update(controller);
		if (status.isChanged()) {
			handleChanged(controller, status);
		} else if (status.isStarted()) {
			handleStarted(controller, status);
		} else if (status.isCrashed()) {
			handleCrashed(controller, status);
		} else {
			handleStatus(controller, status);
		}
	}

	private void validateIOStatus(Controller controller, BaseStatus status) {
		if (controller.getInput() > 0 && controller.getOutput() > 0) {
			if (status.getInputs() == null || status.getInputs().size() < controller.getInput()) {
				throw new MessageFormatException("Inputs size is not valid");
			}
			if (status.getOutputs() == null || status.getOutputs().size() < controller.getOutput()) {
				throw new MessageFormatException("Outputs size is not valid");
			}
		}
	}

	private void handleStatus(Controller controller, BaseStatus status) {
		for (Relay relay : controller.getRelays()) {
			if (relay.updateStatus(status.getOutputs().get(relay.getIndex()))) {
				relayService.update(relay, status.getOutputs().get(relay.getIndex()));
			}
		}
		if (controller.getSwitchs() == null || controller.getSwitchs().isEmpty()) {
			insertSwitch(controller);
		}
		for (Switch zwitch : controller.getSwitchs()) {
			boolean value = status.getInputs().get(zwitch.getIndex());
			if (zwitch.updateStatus(value)) {
				controllerService.updateSwitch(zwitch);
			}
		}
	}

	private void handleStarted(Controller controller, BaseStatus status) {
		LOGGER.info("Relay controller has been started " + controller.getKey());
		if (!controller.isLazy()) {
			handleStatus(controller, status);
		}
		else {
			LOGGER.info("Physical status: " +  Arrays.toString(status.getOutputs().toArray()));
			LOGGER.info("Restore relay status from database to physical relay...");
			for (final Relay relay : controller.getRelays()) {
				final Boolean newStatus = status.getOutputs().get(relay.getIndex());
				if (relay.getStatus() != null && !relay.getStatus().equals(newStatus)) {
					restoreRelayStatus(relay);
				}
			}
		}
	}

	private void restoreRelayStatus(Relay relay) throws ControllerRequestException {
		LOGGER.info("Restore relay status: " + relay.getName() + ", index " + relay.getIndex() + ", status " + relay.getStatus());
		if (relay.getStatus() == false) {
			controlService.switchRelayToOff(relay);
		}
		else if (relay.getStatus() == true) {
			controlService.switchRelayToOn(relay);
		}
	}

	private void handleCrashed(Controller controller, BaseStatus status) {
		handleStatus(controller, status);
		if (controller.getCrashCount() == null) {
			controller.setCrashCount(1);
		}
		else {
			controller.setCrashCount(controller.getCrashCount() + 1);
		}
		controllerService.update(controller);
	}

	private void handleChanged(Controller controller, BaseStatus status) {
		LOGGER.info("Relay controller IO status changed " + controller.getKey());
		for (Relay relay : controller.getRelays()) {
			if (relay.updateStatus(status.getOutputs().get(relay.getIndex()))) {
				relayService.update(relay, relay.getStatus());
				handleRelayStatusChanged(relay);
			}
		}
		if (controller.getSwitchs() == null || controller.getSwitchs().isEmpty()) {
			insertSwitch(controller);
		}
		for (Switch zwitch : controller.getSwitchs()) {
			boolean value = status.getInputs().get(zwitch.getIndex());
			if (zwitch.updateStatus(value)) {
				controllerService.updateSwitch(zwitch);
				handleSwitchStatusChanged(zwitch);
			}
		}
	}

	private void handleSwitchStatusChanged(Switch zwitch) {
		List<Sensor> sensors = sensorDao.findBySwitchId(zwitch.getId());
		if (sensors == null || sensors.isEmpty()) {
			return;
		}
		for (Sensor sensor : sensors) {
			onSensorStatusChanged(sensor);
		}
	}

	@Async
	private void handleRelayStatusChanged(Relay relay) {
		relayStatusNotifier.notifyListeners(new RelayStatusChangedEvent(relay));
	}

	@Async
	private void onSensorStatusChanged(Sensor sensor) {
		sensorStatusNotifier.notifyListeners(new SensorStatusChangedEvent(sensor));
	}

	private void insertSwitch(Controller controller) {
		controller.setSwitchs(new ArrayList<>());
		for (int i = 0; i < controller.getInput(); i++) {
			Switch zwitch = new Switch();
			zwitch.setDriver(controller);
			zwitch.setIndex(i);
			switchDao.insert(zwitch);
			controller.getSwitchs().add(zwitch);
		}
	}

}
