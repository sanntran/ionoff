package net.xapxinh.center.server.api.player;

public class ConnectionClosedEvent extends ConnectionEvent {
	
	public ConnectionClosedEvent(PlayerConnection connection) {
		super(connection);
	}
}
 