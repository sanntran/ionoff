package net.ionoff.center.server.relaydriver.api;

public class RecievedMessageEvent extends ConnectionEvent {
	
	public RecievedMessageEvent(RelayDriverConnection connection) {
		super(connection);
	}
}
 