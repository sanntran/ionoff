package net.xapxinh.center.server.api.player;

import java.util.Map;

public class RecievedMessageEvent extends ConnectionEvent {
	
	private final Map<String, String> messages;
	
	public RecievedMessageEvent(PlayerConnection connection, Map<String, String> messages) {
		super(connection);
		this.messages = messages;
	}

	public Map<String, String> getMessages() {
		return messages;
	}
}
 