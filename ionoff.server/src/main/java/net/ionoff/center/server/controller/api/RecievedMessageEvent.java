package net.ionoff.center.server.controller.api;

public class RecievedMessageEvent extends ConnectionEvent {
	
	public RecievedMessageEvent(ControllerConnection connection) {
		super(connection);
	}
}
 