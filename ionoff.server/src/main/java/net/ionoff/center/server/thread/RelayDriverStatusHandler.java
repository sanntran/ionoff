package net.ionoff.center.server.thread;

import java.util.List;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.Switch;
import net.ionoff.center.server.notifier.RelayStatusNotifier;
import net.ionoff.center.server.notifier.SensorStatusNotifier;
import net.ionoff.center.server.notifier.event.RelayStatusChangedEvent;
import net.ionoff.center.server.notifier.event.SensorStatusChangedEvent;
import net.ionoff.center.server.persistence.service.IRelayDriverService;
import net.ionoff.center.server.persistence.service.IRelayService;
import net.ionoff.center.server.persistence.service.ISensorService;
import net.ionoff.center.server.relaydriver.api.RelayDriverStatus;
import net.ionoff.center.shared.entity.RelayDriverModel;

public class RelayDriverStatusHandler {
	
	private static final Logger LOGGER = Logger.getLogger(RelayDriverStatusHandler.class.getName());
	
	@Autowired
	private IRelayDriverService relayDriverService;
	
	@Autowired
	private IRelayService relayService;
	
	@Autowired
	private ISensorService sensorService;
	
	@Autowired
	private SensorStatusNotifier sensorStatusNotifier;
	
	@Autowired
	private RelayStatusNotifier relayStatusNotifier;
	
	void onRelayDriverStarted(RelayDriver relayDriver, String relayDriverIp, String inStatus, String outStatus) {
		relayDriver.setIp(relayDriverIp);
		relayDriverService.update(relayDriver);
		onReceivedRelayDriverStatus(relayDriver, inStatus, outStatus);
	}

	RelayDriverStatus parseRelayDriverStatus(RelayDriver relayDriver, String inStatus, String outStatus) {
		RelayDriverModel relayDriverModel = relayDriver.getModelObj();
		if (relayDriverModel == null) {
			LOGGER.error("Unknown relayDriver model: " + relayDriver.getModel());
			return null;
		}
		RelayDriverStatus relayDriverStatus = RelayDriverStatus.fromIOStatusesString(relayDriver.getModelObj(), inStatus,
				outStatus);
		if (relayDriverStatus.getDigitalInputStatus().size() < relayDriverModel.getDigitalInput()
				|| relayDriverStatus.getRelayOutputStatus().size() < relayDriverModel.getRelayOutput()) {
			LOGGER.error("Inputs or outputs format is not valid: " + inStatus + " ; " + outStatus);
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
		for (Boolean in : relayDriverStatus.getDigitalInputStatus()) {
			if (in == null) {
				LOGGER.error("Inputs format is not valid: " + inStatus);
				return null;
			}
		}
		for (Boolean out : relayDriverStatus.getRelayOutputStatus()) {
			if (out == null) {
				LOGGER.error("Outputs format is not valid: " + outStatus);
				return null;
			}
		}
		return relayDriverStatus;
	}

	void onReceivedRelayDriverStatus(RelayDriver relayDriver, String inStatus, String outStatus) {
		 
		RelayDriverStatus relayDriverStatus = parseRelayDriverStatus(relayDriver, inStatus, outStatus);
		if (relayDriverStatus == null) {
			LOGGER.error("Invalid relayDriver status data format");
		}
		// Update output status
		for (Relay relay : relayDriver.getRelays()) {
			if (relay.updateStatus(relayDriverStatus.getRelayOutputStatus().get(relay.getIndex()))) {
				// LOGGER.info("Update relay status " + relay.getNameId() + ": "
				// + relayDriverStatus.getRelayOutputStatus().get(relay.getIndex()));
				relayService.update(relay, relayDriverStatus.getRelayOutputStatus().get(relay.getIndex()));
			}
		}
		if (relayDriver.getSwitchs() != null) {
			for (Switch zwitch : relayDriver.getSwitchs()) {
				if (zwitch.updateStatus(relayDriverStatus.getDigitalInputStatus().get(zwitch.getIndex()))) {
					// Sensor status is updated when update switch
					relayDriverService.updateSwitch(zwitch);
				}
			}
		}
		
	}

	void onRelayDriverStatusChanged(RelayDriver relayDriver, String inStatus, String outStatus) {
		RelayDriverStatus relayDriverStatus = parseRelayDriverStatus(relayDriver, inStatus, outStatus);
		if (relayDriverStatus == null) {
			LOGGER.error("Invalid relayDriver status data format");
		}
		// Update output status
		for (Relay relay : relayDriver.getRelays()) {
			if (relay.updateStatus(relayDriverStatus.getRelayOutputStatus().get(relay.getIndex()))) {
				relayService.update(relay);
				onRelayStatusChanged(relay);
			}
		}
		if (relayDriver.getSwitchs() != null) {
			for (Switch zwitch : relayDriver.getSwitchs()) {
				if (zwitch.updateStatus(relayDriverStatus.getDigitalInputStatus().get(zwitch.getIndex()))) {
					// Sensor status is updated when update switch
					relayDriverService.updateSwitch(zwitch);
					onSwitchStatusChanged(zwitch);
				}
			}
		}
		
	}

	private void onSwitchStatusChanged(Switch zwitch) {
		List<Sensor> sensors = sensorService.findBySwitchId(zwitch.getId());
		if (sensors == null || sensors.isEmpty()) {
			return;
		}
		for (Sensor sensor : sensors) {
			onSensorStatusChanged(sensor);
		}
	}

	private void onRelayStatusChanged(Relay relay) {
		relayStatusNotifier.notifyListeners(new RelayStatusChangedEvent(relay));
	}

	private void onSensorStatusChanged(Sensor sensor) {
		new Thread() {
			@Override
			public void run() {
				sensorStatusNotifier.notifyListeners(new SensorStatusChangedEvent(sensor));
			}
		}.start();
	}

	public void onRelayDriverCrashed(RelayDriver relayDriver, String in, String out) {
		onReceivedRelayDriverStatus(relayDriver, in, out);
		if (relayDriver.getCrashCount() == null) {
			relayDriver.setCrashCount(1);
		}
		else {
			relayDriver.setCrashCount(relayDriver.getCrashCount() + 1);
		}
		relayDriverService.update(relayDriver);
	}
}
