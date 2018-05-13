package net.ionoff.center.server.message.event;

import net.ionoff.center.server.entity.Relay;

public class RelayStatusChangedEvent {
	
	private final Relay relay;
	
	public RelayStatusChangedEvent(Relay relay) {
		this.relay = relay;
	}
	
	public Relay getRelay() {
		return relay;
	}
}
 