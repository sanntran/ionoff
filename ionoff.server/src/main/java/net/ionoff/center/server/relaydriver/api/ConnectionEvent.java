package net.ionoff.center.server.relaydriver.api;

public class ConnectionEvent {
	
	private final RelayDriverConnection connection;
	
	public ConnectionEvent(RelayDriverConnection connection) {
		this.connection = connection;	
	}
	
	public RelayDriverConnection getConnection() {
		return connection;
	}
}
 