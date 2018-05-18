package net.ionoff.center.server.exception;

public class RelayLockedException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public RelayLockedException(String message) {
		super(message);
	}
}
