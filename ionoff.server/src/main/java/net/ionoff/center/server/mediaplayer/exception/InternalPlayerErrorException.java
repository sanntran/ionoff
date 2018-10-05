package net.ionoff.center.server.mediaplayer.exception;

public class InternalPlayerErrorException extends MediaPlayerRequestException {

	private static final long serialVersionUID = 1L;

	public InternalPlayerErrorException (String message, Throwable t) {
		super(message, t);
	}
}
