package net.ionoff.center.server.entity;

import java.util.List;

public class Light extends Device {

	private static final long serialVersionUID = 1L;

	private Boolean status;

	@Override
	public Boolean getStatus() {
		return status;
	}

	public void setStatus(Boolean status) {
		this.status = status;
	}

	public Relay getRelay() {
		final List<Relay> relays = getRelayList();
		if (relays.isEmpty()) {
			return null;
		}
		return relays.get(0);
	}
}
