package net.ionoff.broker.http;

public enum HttpMethod {
    GET,
    PUT,
    POST,
    DELETE,
    HEAD,
    OPTIONS,
    TRACE,
    CONNECT,
    PATCH,
    PROPFIND,
    PROPPATCH,
    MKCOL,
    MOVE,
    COPY,
    LOCK,
    UNLOCK,
    NOTIFY,
    SUBSCRIBE;

    public static HttpMethod lookup(String method) {
        if (method == null)
            return null;

        try {
            return valueOf(method);
        } catch (IllegalArgumentException e) {
            // TODO: Log it?
            return null;
        }
    }
}
