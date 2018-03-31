package net.ionoff.center.server.entity;

public class Appliance extends Device {
	private static final long serialVersionUID = 1L;

	public boolean isOn() {
		final Boolean status = getStatus();
		return status != null && status.booleanValue() == true;
	}

	public boolean isOff() {
		final Boolean status = getStatus();
		return status != null && status.booleanValue() == false;
	}
}
