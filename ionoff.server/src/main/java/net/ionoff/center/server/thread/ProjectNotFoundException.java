package net.ionoff.center.server.thread;

public class ProjectNotFoundException extends Exception {

	private static final long serialVersionUID = 1L;

	public ProjectNotFoundException() {
		super();
	}

	public ProjectNotFoundException(String message) {
		super(message);
	}
}
