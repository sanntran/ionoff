package net.ionoff.center.server.exception;

public class MqttConnectionException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public MqttConnectionException(String message) {
		super(message);
	}
}
