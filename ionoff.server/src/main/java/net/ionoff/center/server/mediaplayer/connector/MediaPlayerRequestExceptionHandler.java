package net.ionoff.center.server.mediaplayer.connector;

import com.google.gson.Gson;
import net.ionoff.center.server.broker.BrokerResponse;
import net.ionoff.center.server.mediaplayer.exception.MediaPlayerConnectException;
import net.ionoff.center.server.mediaplayer.exception.MediaPlayerRequestException;
import net.ionoff.center.server.wsclient.RestTemplateConnectException;
import net.ionoff.center.server.wsclient.RestTemplateExceptionHandler;
import net.ionoff.center.server.wsclient.RestTemplateRequestException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

public class MediaPlayerRequestExceptionHandler extends RestTemplateExceptionHandler {

    private final Gson gson = new Gson();

    @Override
    protected RuntimeException buildRestTemplateRequestException(HttpStatus statusCode,
                                                                             HttpHeaders headers, String responseBody) {
        BrokerResponse brokerResponse = gson.fromJson(responseBody, BrokerResponse.class);
        if ("TimeoutException".equals(brokerResponse.getCode())) {
            return new MediaPlayerConnectException(String.valueOf(brokerResponse.getData()));
        }
        return new MediaPlayerRequestException(String.valueOf(brokerResponse.getData()));
    }

    @Override
    protected RuntimeException newRestTemplateConnectException() {
        return new MediaPlayerConnectException();
    }
}