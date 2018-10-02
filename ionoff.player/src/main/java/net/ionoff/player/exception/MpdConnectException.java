package net.ionoff.player.exception;

public class MpdConnectException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public MpdConnectException() {
		super();
	}

	public MpdConnectException(String message) {
		super(message);
	}
}
