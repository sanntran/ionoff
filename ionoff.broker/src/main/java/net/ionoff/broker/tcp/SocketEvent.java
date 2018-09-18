package net.ionoff.broker.tcp;

public class SocketEvent {
	
	private final SocketHandler connection;

	public SocketEvent(SocketHandler connection) {
		this.connection = connection;
	}

	public SocketHandler getConnection() {
		return connection;
	}
}
