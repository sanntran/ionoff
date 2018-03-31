package net.xapxinh.center.server.exception;

public class PlayerConnectException extends PlayServerException {

	private static final long serialVersionUID = 1L;

	public PlayerConnectException() {
		super();
	}

	public PlayerConnectException(String playerId) {
		super(playerId);
	}
}
