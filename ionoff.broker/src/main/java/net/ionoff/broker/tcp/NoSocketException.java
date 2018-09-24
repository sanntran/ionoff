package net.ionoff.broker.tcp;

public class NoSocketException extends RuntimeException {

	private static final long serialVersionUID = 1L;
	
	public NoSocketException(String name) {
		super("No socket connection: " + name);
	}
}
