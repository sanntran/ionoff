package net.ionoff.center.server.relaydriver.api;

public class ConnectionClosedEvent extends ConnectionEvent {
	
	public ConnectionClosedEvent(RelayDriverConnection connection) {
		super(connection);
	}
}
 