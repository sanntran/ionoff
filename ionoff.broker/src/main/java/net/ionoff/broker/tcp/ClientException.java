package net.ionoff.broker.tcp;

import net.ionoff.broker.http.HttpStatus;

public class ClientException extends RuntimeException {

    private HttpStatus status;

    public ClientException(HttpStatus status, String message) {
        super(message);
        this.status = status;
    }

    public HttpStatus getStatus() {
        return status;
    }
}
