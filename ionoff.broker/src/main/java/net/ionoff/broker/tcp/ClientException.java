package net.ionoff.broker.tcp;

public class ClientException extends RuntimeException {

    private int status;

    public ClientException(int status, String message) {
        super(message);
        this.status = status;
    }

    public int getStatus() {
        return status;
    }
}
