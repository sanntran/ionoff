package net.ionoff.center.shared.entity;

public enum ControllerModel {
	
	IONOFF_E4(4, 4), IONOFF_P8(8, 8), IONOFF_P4(4, 4), HBQ_EC100(28, 28), HLAB_EP2(0, 20);
	
	private int digitalInput;	
	private int relayOutput;

	ControllerModel(int digitalInput, int relayOutput) {
		this.digitalInput = digitalInput;
		this.relayOutput = relayOutput;
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
	
	public static ControllerModel fromString(String modelName) {
		if (IONOFF_E4.toString().equals(modelName)) {
			return IONOFF_E4;
		}
		if (IONOFF_P4.toString().equals(modelName)) {
			return IONOFF_P4;
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
		return null;
	}
}
