package net.ionoff.center.server.thread;

import java.util.List;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.controller.api.ControllerStatus;
import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.Switch;
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
		if (controller.getSwitchs() != null) {
			for (Switch zwitch : controller.getSwitchs()) {
				if (zwitch.updateStatus(controllerStatus.getDigitalInputStatus().get(zwitch.getIndex()))) {
					// Sensor status is updated when update switch
					controllerService.updateSwitch(zwitch);
				}
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
		if (controller.getSwitchs() != null) {
			for (Switch zwitch : controller.getSwitchs()) {
				if (zwitch.updateStatus(controllerStatus.getDigitalInputStatus().get(zwitch.getIndex()))) {
					// Sensor status is updated when update switch
					controllerService.updateSwitch(zwitch);
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
