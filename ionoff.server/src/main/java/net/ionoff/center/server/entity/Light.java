package net.ionoff.center.server.entity;

import java.util.List;

public class Light extends Device {

	private static final long serialVersionUID = 1L;

	public Relay getRelay() {
		final List<Relay> relays = getRelayList();
		if (relays.isEmpty()) {
			return null;
		}
		return relays.get(0);
	}
	

	public boolean isOn() {
		final Boolean status = getStatus();
		return status != null && status.booleanValue() == true;
	}

	public boolean isOff() {
		final Boolean status = getStatus();
		return status != null && status.booleanValue() == false;
	}
}
