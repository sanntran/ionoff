package net.ionoff.center.server.thread;

import java.util.List;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.controller.api.ControllerStatus;
import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.notifier.RelayStatusNotifier;
import net.ionoff.center.server.notifier.SensorStatusNotifier;
import net.ionoff.center.server.notifier.event.RelayStatusChangedEvent;
import net.ionoff.center.server.notifier.event.SensorStatusChangedEvent;
import net.ionoff.center.server.persistence.service.IControllerService;
import net.ionoff.center.server.persistence.service.IRelayService;
import net.ionoff.center.server.persistence.service.ISensorService;
import net.ionoff.center.shared.entity.ControllerModel;

public class ControllerStatusHandler {
	
	private static final Logger LOGGER = Logger.getLogger(ControllerStatusHandler.class.getName());
	
	@Autowired
	private IControllerService controllerService;
	@Autowired
	private IRelayService relayService;
	@Autowired
	private ISensorService sensorService;
	@Autowired
	private SensorStatusNotifier sensorStatusNotifier;
	@Autowired
	private RelayStatusNotifier relayStatusNotifier;
	
	void onControllerStarted(Controller controller, String controllerIp, String inStatus, String outStatus) {
		controller.setIp(controllerIp);
		controllerService.update(controller);
		onReceivedControllerStatus(controller, inStatus, outStatus);
	}

	ControllerStatus parseControllerStatus(Controller controller, String inStatus, String outStatus) {
		ControllerModel controllerModel = controller.getModelObj();
		if (controllerModel == null) {
			LOGGER.error("Unknown controller model: " + controller.getModel());
			return null;
		}
		ControllerStatus controllerStatus = ControllerStatus.fromIOStatusesString(controller.getModelObj(), inStatus,
				outStatus);
		if (controllerStatus.getDigitalInputStatus().size() < controllerModel.getDigitalInput()
				|| controllerStatus.getRelayOutputStatus().size() < controllerModel.getRelayOutput()) {
			LOGGER.error("Inputs or outputs format is not valid: " + inStatus + " ; " + outStatus);
			return null;
		}
		if (controllerStatus.getDigitalInputStatus().size() > controllerModel.getDigitalInput()) {
			for (int i = controllerStatus.getDigitalInputStatus().size() - 1; i >= controllerModel
					.getDigitalInput(); i--) {
				controllerStatus.getDigitalInputStatus().remove(i);
			}
		}
		if (controllerStatus.getRelayOutputStatus().size() > controllerModel.getRelayOutput()) {
			for (int i = controllerStatus.getRelayOutputStatus().size() - 1; i >= controllerModel
					.getRelayOutput(); i--) {
				controllerStatus.getRelayOutputStatus().remove(i);
			}
		}
		for (Boolean in : controllerStatus.getDigitalInputStatus()) {
			if (in == null) {
				LOGGER.error("Inputs format is not valid: " + inStatus);
				return null;
			}
		}
		for (Boolean out : controllerStatus.getRelayOutputStatus()) {
			if (out == null) {
				LOGGER.error("Outputs format is not valid: " + outStatus);
				return null;
			}
		}
		return controllerStatus;
	}

	void onReceivedControllerStatus(Controller controller, String inStatus, String outStatus) {
		
		ControllerStatus controllerStatus = parseControllerStatus(controller, inStatus, outStatus);
		if (controllerStatus == null) {
			LOGGER.error("Invalid controller status data format");
		}
		// Update output status
		for (Relay relay : controller.getRelays()) {
			if (relay.updateStatus(controllerStatus.getRelayOutputStatus().get(relay.getIndex()))) {
				// LOGGER.info("Update relay status " + relay.getNameId() + ": "
				// + controllerStatus.getRelayOutputStatus().get(relay.getIndex()));
				relayService.update(relay, controllerStatus.getRelayOutputStatus().get(relay.getIndex()));
			}
		}
		// Update sensor status
		List<Sensor> sensors = sensorService.findByControllerId(controller.getId());
		if (sensors.isEmpty()) {
			return;
		}
		for (Sensor sensor : sensors) {
			if (isSensorStatusChanged(sensor, controllerStatus.getDigitalInputStatus())) {
				sensor.setStatus(controllerStatus.getDigitalInputStatus(sensor.getControllerInput()));
				sensorService.update(sensor);
			}
		}
	}

	void onControllerStatusChanged(Controller controller, String inStatus, String outStatus) {
		ControllerStatus controllerStatus = parseControllerStatus(controller, inStatus, outStatus);
		if (controllerStatus == null) {
			LOGGER.error("Invalid controller status data format");
		}
		// Update output status
		for (Relay relay : controller.getRelays()) {
			if (relay.updateStatus(controllerStatus.getRelayOutputStatus().get(relay.getIndex()))) {
				relayService.update(relay);
				onRelayStatusChanged(relay);
			}
		}
		// Update sensor status
		List<Sensor> sensors = sensorService.findByControllerId(controller.getId());
		for (Sensor sensor : sensors) {
			if (isSensorStatusChanged(sensor, controllerStatus.getDigitalInputStatus())) {
				onSensorStatusChanged(sensor,
						controllerStatus.getDigitalInputStatus().get(sensor.getControllerInput()));
			}
		}
	}

	private void onRelayStatusChanged(Relay relay) {
		relayStatusNotifier.notifyListeners(new RelayStatusChangedEvent(relay));
	}

	void onSensorStatusChanged(Sensor sensor, Boolean newStatus) {
		if (newStatus == null) {
			return;
		}
		if (newStatus.booleanValue() == false) {
			onSensorDetectedHuman(sensor); // digital input is 1 means that it
											// is triggered
		} else if (newStatus.booleanValue() == true) {
			onSensorDetectedNoHuman(sensor); // digital input is 1 means that it
												// is not triggered
		}
	}

	void onSensorDetectedHuman(Sensor sensor) {
		sensor.setStatus(true); // set the sensor to triggered
		sensorService.update(sensor);
		new Thread() {
			@Override
			public void run() {
				sensorStatusNotifier.notifyListeners(new SensorStatusChangedEvent(sensor));
			}
		}.start();
	}

	void onSensorDetectedNoHuman(Sensor sensor) {
		sensor.setStatus(false); // set the sensor to default not triggered
		sensorService.update(sensor);
		new Thread() {
			@Override
			public void run() {
				sensorStatusNotifier.notifyListeners(new SensorStatusChangedEvent(sensor));
			}
		}.start();
	}

	boolean isSensorStatusChanged(Sensor sensor, List<Boolean> digitalInputStatus) {
		if (sensor.getControllerInput() != null
				&& (sensor.getControllerInput().intValue() < digitalInputStatus.size())) {
			return sensor.getStatus() == null
					// it is very important to aware that sensor status is
					// !digital input status
					// so when their status are equal that mean status changed
					|| (sensor.getStatus().booleanValue() == digitalInputStatus.get(sensor.getControllerInput()));
		}
		return false;
	}

	public void onControllerCrashed(Controller controller, String in, String out) {
		onReceivedControllerStatus(controller, in, out);
		if (controller.getCrashCount() == null) {
			controller.setCrashCount(1);
		}
		else {
			controller.setCrashCount(controller.getCrashCount() + 1);
		}
		controllerService.update(controller);
	}
}
