package net.ionoff.broker.tcp;

import java.util.HashMap;
import java.util.Map;

public class HttpResponse {

    private int status;
    private String method;
    private String path;
    private Map<String, String> params;
    private String body;

    public HttpResponse(String message) {

        params = new HashMap<>();
    }
}
