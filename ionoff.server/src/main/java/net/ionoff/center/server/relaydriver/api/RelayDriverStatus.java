package net.ionoff.center.server.relaydriver.api;

import java.util.ArrayList;
import java.util.List;

import net.ionoff.center.shared.entity.RelayDriverModel;

public class RelayDriverStatus {
	
	private final List<Boolean> relayOutputStatus;
	private final List<Boolean> digitalInputStatus;

	public RelayDriverStatus() {
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
	
	public static RelayDriverStatus fromIOStatusesString(RelayDriverModel relayDriverModel, String inputStatuses, String outputStatuses) {
		RelayDriverStatus relayDriverStatus = new RelayDriverStatus();
		if (inputStatuses != null && outputStatuses != null) {
			
			for (int i = 0; i < relayDriverModel.getDigitalInput(); i++) {
				relayDriverStatus.getDigitalInputStatus().add(i, getStatus(relayDriverModel, inputStatuses.charAt(i)));
			}
			for (int i = 0; i < relayDriverModel.getRelayOutput(); i++) {
				relayDriverStatus.getRelayOutputStatus().add(i, getStatus(relayDriverModel, outputStatuses.charAt(i)));
			}
		}
		return relayDriverStatus;
	}

	private static Boolean getStatus(RelayDriverModel relayDriverModel, char c) {
		if (RelayDriverModel.IONOFF_E4.equals(relayDriverModel) || 
				RelayDriverModel.IONOFF_E3.equals(relayDriverModel)) {
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
