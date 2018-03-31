package net.ionoff.center.server.exception;

public class ChangeEntityIdException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public ChangeEntityIdException(String entity) {
		super("ID cannot be change. " + entity);
	}
}
