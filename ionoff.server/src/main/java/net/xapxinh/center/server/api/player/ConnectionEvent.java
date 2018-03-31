package net.xapxinh.center.server.api.player;

public class ConnectionEvent {
	
	private final PlayerConnection connection;
	
	public ConnectionEvent(PlayerConnection connection) {
		this.connection = connection;	
	}
	
	public PlayerConnection getConnection() {
		return connection;
	}
}
 