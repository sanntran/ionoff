package net.xapxinh.center.server.exception;

public class UnknownPlayerException extends PlayServerException {

	private static final long serialVersionUID = 1L;

	public UnknownPlayerException() {
		super();
	}

	public UnknownPlayerException(String playerId) {
		super(playerId);
	}
}
