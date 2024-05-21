package net.ionoff.center.server.controller;

import com.google.common.collect.ImmutableList;
import lombok.extern.slf4j.Slf4j;
import net.ionoff.center.server.controller.exception.ControllerRequestException;
import net.ionoff.center.server.controller.exception.MessageFormatException;
import net.ionoff.center.server.controller.model.*;
import net.ionoff.center.server.entity.*;
import net.ionoff.center.server.message.RelayStatusNotifier;
import net.ionoff.center.server.message.SensorStatusNotifier;
import net.ionoff.center.server.message.event.RelayStatusChangedEvent;
import net.ionoff.center.server.message.event.SensorStatusChangedEvent;
import net.ionoff.center.server.persistence.dao.IControllerDao;
import net.ionoff.center.server.persistence.dao.IProjectDao;
import net.ionoff.center.server.persistence.dao.ISensorDao;
import net.ionoff.center.server.persistence.dao.ISensorStatusDao;
import net.ionoff.center.server.persistence.service.IControllerService;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.persistence.service.IRelayService;
import net.ionoff.center.server.service.IControlService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import java.util.*;

@Slf4j
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
	private IControllerService controllerService;
	
	@Lazy // very important to be lazy here due to cyclic dependency
	@Autowired
	private SensorStatusNotifier sensorStatusNotifier;
	
	@Lazy // very important to be lazy here due to cyclic dependency
	@Autowired
	private RelayStatusNotifier relayStatusNotifier;

	@Lazy
	@Autowired
	private IProjectDao projectDao;

	@Lazy
	@Autowired
	private ISensorStatusDao sensorStatusDao;

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
		if (SxIOStatus.accept(payload)) {
			handleStatusMessage(SxIOStatus.of(payload));
		} else {
			BaseStatus status = parseIOStatus(payload);
			if (status == null) {
				log.error("Unknown message format {}", payload);
				return;
			}
			handleStatusMessage(status);
		}
	}

	private void handleStatusMessage(SxIOStatus status) {
		String controllerKey =i status.getControllerKey();
		Optional<Controller> controller = controllerDao.findByKey(controllerKey);
		if (!controller.isPresent()) {
			Controller newController = insertController(status);
			log.info("Inserted controller {} as first time connected", newController.getKey());
		} else {
			Controller existingController = controller.get();
			existingController.setLastConnected(System.currentTimeMillis());
			controllerDao.update(existingController);
			for (Sensor sensor : existingController.getSensors()) {
				Double value = "ON".equals(status.getSensors().get(sensor.getIndex()).getValue()) ? 1D : 0D;
				if (sensor.updateStatus(value)) {
					controllerService.updateSensor(sensor);
				}
			}
		}
	}

	private Controller insertController(SxIOStatus status) {
		Project project = projectDao
				.findFirst()
				.orElseThrow(() -> new IllegalStateException("No project found when insert controller"));
		Controller newController = new Controller();
		String id = status.getId();
		newController.setName(id);
		newController.setType("IONOFF_SM");
		newController.setModel("IONOFF_1S");
		newController.setKey(id);
		newController.setProject(project);
		newController.setLastConnected(System.currentTimeMillis());
		newController.setRelays(ImmutableList.of());
		newController.setOnlineBuffer(15000);
		newController.setConnectionExpired(newController.getLastConnected() + newController.getOnlineBuffer());
		List<Sensor> sensors = newSensors(newController, status);
		newController.setSensors(sensors);
		return controllerDao.insert(newController);
	}

	private List<Sensor> newSensors(Controller controller, SxIOStatus status) {
		List<Sensor> sensors = new ArrayList<>();
		for (int i = 0; i < status.getSensors().size(); i++) {
			Sensor sensor = new Sensor();
			sensor.setController(controller);
			sensor.setName(status.getSensors().get(i).getAddrSub());
			sensor.setProject(controller.getProject());
			net.ionoff.center.server.entity.SensorStatus sensorStatus = new net.ionoff.center.server.entity.SensorStatus();
			sensorStatus.setIndex(i);
			sensorStatus.setTime(new Date(status.getSensors().get(i).getTimestamp()));
			sensorStatus.setValue("ON".equals(status.getSensors().get(i).getValue()) ? 1D : 0D);
			sensorStatus.setAlert("ON".equals(status.getSensors().get(i).getValue()));
			sensorStatus.setName(status.getSensors().get(i).getType());
			sensorStatus.setSensor(sensor);
			sensor.setStatus(sensorStatus);
			sensor.setController(controller);
			sensors.add(sensor);
		}
		return sensors;
	}

	private BaseStatus parseIOStatus(String payload) {
		try {
			if (ExIOStatus.accept(payload)) {
				return new ExIOStatus(payload);
			} else if (PxIOStatus.accept(payload)) {
				return new PxIOStatus(payload);
			}
		} catch (Exception e) {
			log.error("Error parsing message {}", e.getMessage());
		}
		return null;
	}

	public void handleStatusMessage(BaseStatus status) {
		Optional<Controller> controllers = controllerDao.findByKey(status.getKey());
		if (!controllers.isPresent()) {
			log.info("No controller found by key {}", status.getKey());
			return;
		}
		Controller controller = controllers.get();
		validateIOStatus(controller, status);
		if (!controller.isConnected()) {
			LOGGER.info("Controller " + controller.getKey() + " is now connected");
			if (controller.isLazy()) {
				controller.setLastConnected(System.currentTimeMillis());
				controllerDao.update(controller);
				handleStarted(controller, status);
				return;
			}
		}
		controller.setLastConnected(System.currentTimeMillis());
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
		for (Sensor sensor : controller.getSensors()) {
			Double value = status.getInputs().get(sensor.getIndex());
			if (sensor.updateStatus(value)) {
				controllerService.updateSensor(sensor);
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
		if (controller.getSensors() == null || controller.getSensors().isEmpty()) {
			insertSensor(controller);
		}
		for (Sensor sensor : controller.getSensors()) {
			Double value = status.getInputs().get(sensor.getIndex());
			if (sensor.updateStatus(value)) {
				controllerService.updateSensor(sensor);
				handleSensorStatusChanged(sensor);
			}
		}
	}

	private void handleSensorStatusChanged(Sensor sensor) {
		onSensorStatusChanged(sensor);
	}

	@Async
    protected void handleRelayStatusChanged(Relay relay) {
		relayStatusNotifier.notifyListeners(new RelayStatusChangedEvent(relay));
	}

	@Async
    protected void onSensorStatusChanged(Sensor sensor) {
		sensorStatusNotifier.notifyListeners(new SensorStatusChangedEvent(sensor));
	}

	private void insertSensor(Controller controller) {
		controller.setSensors(new ArrayList<>());
		for (int i = 0; i < controller.getInput(); i++) {
			Sensor sensor = new Sensor();
			sensor.setController(controller);
			sensor.setIndex(i);
			controller.getSensors().add(sensor);
		}
	}

}
