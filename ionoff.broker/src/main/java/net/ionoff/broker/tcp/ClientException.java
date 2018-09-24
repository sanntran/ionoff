package net.ionoff.broker.tcp;

public class ClientException extends RuntimeException {

    private Status status;

    public ClientException(Status status, String message) {
        super(message);
        this.status = status;
    }

    public Status getStatus() {
        return status;
    }
}
