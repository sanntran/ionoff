package net.ionoff.center.server.controller.api;

public class ConnectionEvent {
	
	private final ControllerConnection connection;
	
	public ConnectionEvent(ControllerConnection connection) {
		this.connection = connection;	
	}
	
	public ControllerConnection getConnection() {
		return connection;
	}
}
 