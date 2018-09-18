package net.ionoff.broker.tcp;

public class MessageArrivedEvent extends SocketEvent {

	private String request;

	public MessageArrivedEvent(String message, SocketHandler connection) {
		super(connection);
		this.request = message;
	}
}
 