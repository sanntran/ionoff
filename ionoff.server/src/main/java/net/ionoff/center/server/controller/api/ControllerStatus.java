package net.ionoff.center.server.controller.api;

import java.util.ArrayList;
import java.util.List;

import net.ionoff.center.shared.entity.ControllerModel;

public class ControllerStatus {
	
	private final List<Boolean> relayOutputStatus;
	private final List<Boolean> digitalInputStatus;

	public ControllerStatus() {
		relayOutputStatus = new ArrayList<Boolean>();
		digitalInputStatus = new ArrayList<Boolean>();
	}

	public List<Boolean> getRelayOutputStatus() {
		return relayOutputStatus;
	}
	
	public Boolean getDigitalInputStatus(int index) {
		return digitalInputStatus.get(index);
	}
	
	public Boolean getRelayOutputStatus(int index) {
		return relayOutputStatus.get(index);
	}

	public List<Boolean> getDigitalInputStatus() {
		return digitalInputStatus;
	}
	
	public static ControllerStatus fromIOStatusesString(ControllerModel controllerModel, String inputStatuses, String outputStatuses) {
		ControllerStatus controllerStatus = new ControllerStatus();
		if (inputStatuses != null && outputStatuses != null) {
			
			for (int i = 0; i < controllerModel.getDigitalInput(); i++) {
				controllerStatus.getDigitalInputStatus().add(i, getStatus(controllerModel, inputStatuses.charAt(i)));
			}
			for (int i = 0; i < controllerModel.getRelayOutput(); i++) {
				controllerStatus.getRelayOutputStatus().add(i, getStatus(controllerModel, outputStatuses.charAt(i)));
			}
		}
		return controllerStatus;
	}

	private static Boolean getStatus(ControllerModel controllerModel, char c) {
		if (ControllerModel.IONOFF_E4.equals(controllerModel)) {
			if ('1' == c) {
				return false;
			}
			if ('0' == c) {
				return true;
			}
		}
		else {
			if ('0' == c) {
				return false;
			}
			if ('1' == c) {
				return true;
			}
		}
		return null;
	}
}
