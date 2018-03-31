package net.ionoff.center.server.exception;

public class MqttPublishException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public MqttPublishException(String message) {
		super(message);
	}
}
