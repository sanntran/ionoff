package net.ionoff.center.server.security;

public class InvalidTokenException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public InvalidTokenException(String token) {
		super("InvalidToken: " + token);
	}
}
