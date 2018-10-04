package net.ionoff.center.server.relaydriver;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import net.ionoff.center.server.control.IControlService;
import net.ionoff.center.server.relaydriver.exception.RelayDriverException;
import net.ionoff.center.server.relaydriver.model.BaseStatus;
import net.ionoff.center.server.relaydriver.model.EcIOStatus;
import net.ionoff.center.server.relaydriver.model.EpIOStatus;
import net.ionoff.center.server.relaydriver.model.ExIOStatus;
import net.ionoff.center.server.relaydriver.model.PxIOStatus;
import net.ionoff.center.server.persistence.dao.IRelayDriverDao;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.relaydriver.exception.DataFormatException;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;

import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.Switch;
import net.ionoff.center.server.message.event.RelayStatusChangedEvent;
import net.ionoff.center.server.message.event.SensorStatusChangedEvent;
import net.ionoff.center.server.message.RelayStatusNotifier;
import net.ionoff.center.server.message.SensorStatusNotifier;
import net.ionoff.center.server.persistence.dao.ISensorDao;
import net.ionoff.center.server.persistence.dao.ISwitchDao;
import net.ionoff.center.server.persistence.service.IRelayDriverService;
import net.ionoff.center.server.persistence.service.IRelayService;

public class RelayDriverHandler {

	private static final Logger LOGGER = Logger.getLogger(RelayDriverHandler.class.getName());

	@Autowired
	private ISensorDao sensorDao;
	
	@Autowired
	private IRelayService relayService;

	@Autowired
	private ISwitchDao switchDao;
	
	@Autowired
	private IRelayDriverService relayDriverService;
	
	@Lazy // very important to be lazy here due to cyclic dependency
	@Autowired
	private SensorStatusNotifier sensorStatusNotifier;
	
	@Lazy // very important to be lazy here due to cyclic dependency
	@Autowired
	private RelayStatusNotifier relayStatusNotifier;


	@Autowired
	private IDeviceService deviceService;

	@Autowired
	private IRelayDriverDao relayDriverDao;


	@Autowired
	IControlService controlService;

	public void onMessageArrived(String payload) {
		BaseStatus status = null;
		try {
			if (ExIOStatus.accept(payload)) {
				status = new ExIOStatus(payload);
			} else if (PxIOStatus.accept(payload)) {
				status = new PxIOStatus(payload);
			} else if (EcIOStatus.accept(payload)) {
				status = new EcIOStatus(payload);
			} else if (EpIOStatus.accept(payload)) {
				status = new EpIOStatus(payload);
			}
		} catch (Exception e) {
			LOGGER.error(e.getClass().getSimpleName() + " Error parsing message " + e.getMessage());
		}
		if (status == null) {
			LOGGER.error("Unknown message format " + payload);
			return;
		}
		handleStatusMessage(status);
	}

	public void handleStatusMessage(BaseStatus status) {
		List<RelayDriver> relayDrivers = relayDriverDao.findByMac(status.getKey());
		if (relayDrivers.isEmpty()) {
			LOGGER.info("No relay-relaydriver found. Key: " + status.getKey());
			return;
		}
		RelayDriver relayDriver = relayDrivers.get(0);
		validateIOStatus(relayDriver, status);
		if (!relayDriver.isConnected()) {
			LOGGER.info("RelayDriver " + relayDriver.getKey() + " is now connected");
			if (!relayDriver.autoPublish()) {
				relayDriver.setConnectedTime(System.currentTimeMillis());
				relayDriverDao.update(relayDriver);
				handleStarted(relayDriver, status);
				return;
			}
		}
		relayDriver.setConnectedTime(System.currentTimeMillis());
		relayDriverDao.update(relayDriver);
		if (status.isChanged()) {
			handleChanged(relayDriver, status);
		} else if (status.isStarted()) {
			handleStarted(relayDriver, status);
		} else if (status.isCrashed()) {
			handleCrashed(relayDriver, status);
		} else {
			handleStatus(relayDriver, status);
		}
	}

	private void validateIOStatus(RelayDriver relayDriver, BaseStatus status) {
		if (status.getInputs().size() < relayDriver.getInput()
				|| status.getOutputs().size() < relayDriver.getOutput()) {
			throw new DataFormatException("Inputs or outputs size is not valid");
		}
	}

	private void handleStatus(RelayDriver relayDriver, BaseStatus status) {
		for (Relay relay : relayDriver.getRelays()) {
			if (relay.updateStatus(status.getOutputs().get(relay.getIndex()))) {
				relayService.update(relay, status.getOutputs().get(relay.getIndex()));
			}
		}
		if (relayDriver.getSwitchs() == null || relayDriver.getSwitchs().isEmpty()) {
			insertSwitch(relayDriver);
		}
		for (Switch zwitch : relayDriver.getSwitchs()) {
			boolean value = status.getInputs().get(zwitch.getIndex());
			if (zwitch.updateStatus(value)) {
				relayDriverService.updateSwitch(zwitch);
			}
		}
	}

	private void handleStarted(RelayDriver relayDriver, BaseStatus status) {
		LOGGER.info("Relay relaydriver has been started " + relayDriver.getKey());
		if (relayDriver.autoPublish()) {
			handleStatus(relayDriver, status);
		}
		else {
			LOGGER.info("Physical status: " +  Arrays.toString(status.getOutputs().toArray()));
			LOGGER.info("Restore relay status from database to physical relay...");
			for (final Relay relay : relayDriver.getRelays()) {
				final Boolean newStatus = status.getOutputs().get(relay.getIndex());
				if (relay.getStatus() != null && !relay.getStatus().equals(newStatus)) {
					restoreRelayStatus(relay);
				}
			}
		}
	}

	private void restoreRelayStatus(Relay relay) throws RelayDriverException {
		LOGGER.info("Restore relay status: " + relay.getName() + ", index " + relay.getIndex() + ", status " + relay.getStatus());
		if (relay.getStatus() == false) {
			controlService.switchRelayToOff(relay);
		}
		else if (relay.getStatus() == true) {
			controlService.switchRelayToOn(relay);
		}
	}

	private void handleCrashed(RelayDriver relayDriver, BaseStatus status) {
		handleStatus(relayDriver, status);
		if (relayDriver.getCrashCount() == null) {
			relayDriver.setCrashCount(1);
		}
		else {
			relayDriver.setCrashCount(relayDriver.getCrashCount() + 1);
		}
		relayDriverService.update(relayDriver);
	}

	private void handleChanged(RelayDriver relayDriver, BaseStatus status) {
		LOGGER.info("Relay relaydriver IO status changed " + relayDriver.getKey());
		for (Relay relay : relayDriver.getRelays()) {
			if (relay.updateStatus(status.getOutputs().get(relay.getIndex()))) {
				relayService.update(relay, relay.getStatus());
				handleRelayStatusChanged(relay);
			}
		}
		if (relayDriver.getSwitchs() == null || relayDriver.getSwitchs().isEmpty()) {
			insertSwitch(relayDriver);
		}
		for (Switch zwitch : relayDriver.getSwitchs()) {
			boolean value = status.getInputs().get(zwitch.getIndex());
			if (zwitch.updateStatus(value)) {
				relayDriverService.updateSwitch(zwitch);
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

	private void insertSwitch(RelayDriver relayDriver) {
		relayDriver.setSwitchs(new ArrayList<>());
		for (int i = 0; i < relayDriver.getInput(); i++) {
			Switch zwitch = new Switch();
			zwitch.setDriver(relayDriver);
			zwitch.setIndex(i);
			switchDao.insert(zwitch);
			relayDriver.getSwitchs().add(zwitch);
		}
	}

}
