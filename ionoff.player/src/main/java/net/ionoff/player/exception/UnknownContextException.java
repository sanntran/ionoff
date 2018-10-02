package net.ionoff.player.exception;

public class UnknownContextException extends BadRequestException {

	private static final long serialVersionUID = 1L;

	public UnknownContextException(String context) {
		super("Unknown context: " + context);
	}
}
