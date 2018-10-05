package net.ionoff.center.server.mediadata.exception;

public class MediaDataRequestException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public MediaDataRequestException() {
		super();
	}

	public MediaDataRequestException(String message) {
		super(message);
	}
}
