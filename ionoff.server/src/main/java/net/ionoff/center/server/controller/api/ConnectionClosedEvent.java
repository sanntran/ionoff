package net.ionoff.center.server.controller.api;

public class ConnectionClosedEvent extends ConnectionEvent {
	
	public ConnectionClosedEvent(ControllerConnection connection) {
		super(connection);
	}
}
 