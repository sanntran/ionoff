package net.ionoff.broker.tcp;

public class ServerException extends RuntimeException {

    private String message;

    public ServerException(String message) {
        super(message);
    }

    public ServerException(String message, Exception e) {
        super(message, e);
    }
}
