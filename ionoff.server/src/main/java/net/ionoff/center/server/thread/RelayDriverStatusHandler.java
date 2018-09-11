package net.ionoff.center.server.thread;

import java.util.ArrayList;
import java.util.List;

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
import net.ionoff.center.server.relaydriver.api.RelayDriverStatus;
import net.ionoff.center.shared.entity.RelayDriverModel;

public class RelayDriverStatusHandler {

	private static final Logger logger = Logger.getLogger(RelayDriverStatusHandler.class.getName());

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
	
	public void onStarted(RelayDriver relayDriver, String ip, String in, String out) {
		relayDriver.setIp(ip);
		relayDriverService.update(relayDriver);
		onReceivedStatus(relayDriver, in, out);
	}

	private RelayDriverStatus parseRelayDriverStatus(RelayDriver relayDriver, String in, String out) {
		RelayDriverModel relayDriverModel = relayDriver.getModelObj();
		if (relayDriverModel == null) {
			logger.error("Unknown relayDriver model: " + relayDriver.getModel());
			return null;
		}
		RelayDriverStatus relayDriverStatus = RelayDriverStatus.fromIOStatusesString(relayDriver.getModelObj(), in,
				out);
		if (relayDriverStatus.getDigitalInputStatus().size() < relayDriverModel.getDigitalInput()
				|| relayDriverStatus.getRelayOutputStatus().size() < relayDriverModel.getRelayOutput()) {
			logger.error("Inputs or outputs format is not valid: " + in + " ; " + out);
			return null;
		}
		if (relayDriverStatus.getDigitalInputStatus().size() > relayDriverModel.getDigitalInput()) {
			for (int i = relayDriverStatus.getDigitalInputStatus().size() - 1; i >= relayDriverModel
					.getDigitalInput(); i--) {
				relayDriverStatus.getDigitalInputStatus().remove(i);
			}
		}
		if (relayDriverStatus.getRelayOutputStatus().size() > relayDriverModel.getRelayOutput()) {
			for (int i = relayDriverStatus.getRelayOutputStatus().size() - 1; i >= relayDriverModel
					.getRelayOutput(); i--) {
				relayDriverStatus.getRelayOutputStatus().remove(i);
			}
		}
		for (Boolean i : relayDriverStatus.getDigitalInputStatus()) {
			if (i == null) {
				logger.error("Inputs format is not valid: " + i);
				return null;
			}
		}
		for (Boolean o : relayDriverStatus.getRelayOutputStatus()) {
			if (o == null) {
				logger.error("Outputs format is not valid: " + o);
				return null;
			}
		}
		return relayDriverStatus;
	}

	public void onReceivedStatus(RelayDriver relayDriver, String in, String out) {
		RelayDriverStatus relayDriverStatus = parseRelayDriverStatus(relayDriver, in, out);
		if (relayDriverStatus == null) {
			logger.error("Invalid relayDriver status data format");
		}
		// Update output status
		for (Relay relay : relayDriver.getRelays()) {
			if (relay.updateStatus(relayDriverStatus.getRelayOutputStatus().get(relay.getIndex()))) {
				// logger.info("Update relay status " + relay.getNameId() + ": "
				// + relayDriverStatus.getRelayOutputStatus().get(relay.getIndex()));
				relayService.update(relay, relayDriverStatus.getRelayOutputStatus().get(relay.getIndex()));
			}
		}
		if (relayDriver.getSwitchs() == null || relayDriver.getSwitchs().isEmpty()) {
			insertSwitch(relayDriver);
		}
		for (Switch zwitch : relayDriver.getSwitchs()) {
			boolean status = relayDriverStatus.getDigitalInputStatus().get(zwitch.getIndex());
			if (zwitch.updateStatus(status)) {
				// Sensor status is updated when update switch
				relayDriverService.updateSwitch(zwitch);
			}
		}
	}

	private void insertSwitch(RelayDriver relayDriver) {
		relayDriver.setSwitchs(new ArrayList<>());
		for (int i = 0; i < relayDriver.getModelObj().getDigitalInput(); i++) {
			Switch zwitch = new Switch();
			zwitch.setDriver(relayDriver);
			zwitch.setIndex(i);
			switchDao.insert(zwitch);
			relayDriver.getSwitchs().add(zwitch);
		}
	}

	public void onStatusChanged(RelayDriver relayDriver, String in, String out) {
		RelayDriverStatus relayDriverStatus = parseRelayDriverStatus(relayDriver, in, out);
		if (relayDriverStatus == null) {
			logger.error("Invalid relayDriver status data format");
		}
		// Update output status
		for (Relay relay : relayDriver.getRelays()) {
			if (relay.updateStatus(relayDriverStatus.getRelayOutputStatus().get(relay.getIndex()))) {
				relayService.update(relay, relay.getStatus());
				onRelayStatusChanged(relay);
			}
		}
		if (relayDriver.getSwitchs() == null || relayDriver.getSwitchs().isEmpty()) {
			insertSwitch(relayDriver);
		}
		for (Switch zwitch : relayDriver.getSwitchs()) {
			boolean status = relayDriverStatus.getDigitalInputStatus().get(zwitch.getIndex());
			if (zwitch.updateStatus(status)) {
				// Sensor status is updated when update switch
				relayDriverService.updateSwitch(zwitch);
				onSwitchStatusChanged(zwitch);
			}
		}
	}

	private void onSwitchStatusChanged(Switch zwitch) {
		List<Sensor> sensors = sensorDao.findBySwitchId(zwitch.getId());
		if (sensors == null || sensors.isEmpty()) {
			return;
		}
		for (Sensor sensor : sensors) {
			onSensorStatusChanged(sensor);
		}
	}

	@Async
	private void onRelayStatusChanged(Relay relay) {
		relayStatusNotifier.notifyListeners(new RelayStatusChangedEvent(relay));
	}

	@Async
	private void onSensorStatusChanged(Sensor sensor) {
		sensorStatusNotifier.notifyListeners(new SensorStatusChangedEvent(sensor));
	}

	public void onCrashed(RelayDriver relayDriver, String in, String out) {
		onReceivedStatus(relayDriver, in, out);
		if (relayDriver.getCrashCount() == null) {
			relayDriver.setCrashCount(1);
		}
		else {
			relayDriver.setCrashCount(relayDriver.getCrashCount() + 1);
		}
		relayDriverService.update(relayDriver);
	}
	
}
