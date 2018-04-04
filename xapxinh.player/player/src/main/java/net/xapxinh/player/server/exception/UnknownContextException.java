package net.xapxinh.player.server.exception;

public class UnknownContextException extends Exception {

	private static final long serialVersionUID = 1L;

	public UnknownContextException(String context) {
		super("Unknown context: " + context);
	}
}
