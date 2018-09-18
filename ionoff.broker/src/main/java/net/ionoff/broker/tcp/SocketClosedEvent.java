package net.ionoff.broker.tcp;

public class SocketClosedEvent extends SocketEvent {
	
	public SocketClosedEvent(SocketHandler connection) {
		super(connection);
	}
}
 