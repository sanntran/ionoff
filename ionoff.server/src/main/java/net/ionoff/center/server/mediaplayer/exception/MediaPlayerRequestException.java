package net.ionoff.center.server.mediaplayer.exception;

public class MediaPlayerRequestException extends RuntimeException {

    public MediaPlayerRequestException() {
        super();
    }

    public MediaPlayerRequestException(String message) {
        super(message);
    }

    public MediaPlayerRequestException(String message, Throwable t) {
        super(message, t);
    }
}
