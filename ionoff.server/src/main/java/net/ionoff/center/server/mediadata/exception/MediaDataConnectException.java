package net.ionoff.center.server.mediadata.exception;


public class MediaDataConnectException extends MediaDataRequestException {

	private static final long serialVersionUID = 1L;

	public MediaDataConnectException() {
		super();
	}

	public MediaDataConnectException(String playerId) {
		super(playerId);
	}
}
