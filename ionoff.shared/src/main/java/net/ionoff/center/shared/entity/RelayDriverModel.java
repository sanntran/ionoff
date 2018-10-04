package net.ionoff.center.shared.entity;

public enum RelayDriverModel {
	
	IONOFF_E3(3, 3, 49000), 
	IONOFF_E4(4, 4, 49000), 
	IONOFF_P8(8, 8, 32000), 
	IONOFF_P4(4, 4, 32000),
	HBQ_EC100(28, 28, 15000), 
	HLAB_EP2(0, 20, 15000);
	
	
	private int digitalInput;	
	private int relayOutput;
	private long onlineBuffer;

	RelayDriverModel(int digitalInput, int relayOutput, long onlineBuffer) {
		this.digitalInput = digitalInput;
		this.relayOutput = relayOutput;
		this.onlineBuffer = onlineBuffer;
	}
	
	public int getRelayOutput() {
		return relayOutput;
	}

	public int getDigitalInput() {
		return digitalInput;
	}
	
	public static RelayDriverModel fromString(String modelName) {
		for (RelayDriverModel model : RelayDriverModel.values()) {
			if (model.toString().equals(modelName)) {
				return model;
			}
		}
		return null;
	}

	public long getOnlineBuffer() {
		return onlineBuffer;
	}
}
