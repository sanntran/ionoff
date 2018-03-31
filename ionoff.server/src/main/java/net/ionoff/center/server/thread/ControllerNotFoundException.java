package net.ionoff.center.server.thread;

public class ControllerNotFoundException extends Exception {

	private static final long serialVersionUID = 1L;

	public ControllerNotFoundException() {
		super();
	}

	public ControllerNotFoundException(String message) {
		super(message);
	}
}