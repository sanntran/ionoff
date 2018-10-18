package net.ionoff.center.server.controller.connector;

import com.google.gson.Gson;
import net.ionoff.center.server.broker.BrokerResponse;
import net.ionoff.center.server.controller.exception.ControllerConnectException;
import net.ionoff.center.server.wsclient.RestTemplateExceptionHandler;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

public class ControllerRequestExceptionHandler extends RestTemplateExceptionHandler {

    private final Gson gson = new Gson();

    @Override
    protected RuntimeException buildRestTemplateRequestException(HttpStatus statusCode,
                                                            HttpHeaders headers, String responseBody) {

        BrokerResponse brokerResponse = gson.fromJson(responseBody, BrokerResponse.class);
        if ("TimeoutException".equals(brokerResponse.getCode())) {
            return new ControllerConnectException(String.valueOf(brokerResponse.getData()));
        }
        return new ControllerConnectException(String.valueOf(brokerResponse.getData()));
    }
}