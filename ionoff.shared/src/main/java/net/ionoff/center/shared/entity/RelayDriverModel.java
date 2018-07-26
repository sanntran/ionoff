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
	public void setRelayOutput(int relayOutput) {
		this.relayOutput = relayOutput;
	}

	public void setDigitalInput(int digitalInput) {
		this.digitalInput = digitalInput;
	}
	
	public static RelayDriverModel fromString(String modelName) {
		if (IONOFF_E3.toString().equals(modelName)) {
			return IONOFF_E3;
		}
		if (IONOFF_E4.toString().equals(modelName)) {
			return IONOFF_E4;
		}
		if (IONOFF_P8.toString().equals(modelName)) {
			return IONOFF_P8;
		}
		if (HBQ_EC100.toString().equals(modelName)) {
			return HBQ_EC100;
		}
		if (HLAB_EP2.toString().equals(modelName)) {
			return HLAB_EP2;
		}
		if (IONOFF_P4.toString().equals(modelName)) {
			return IONOFF_P4;
		}
		return null;
	}

	public long getOnlineBuffer() {
		return onlineBuffer;
	}
}
