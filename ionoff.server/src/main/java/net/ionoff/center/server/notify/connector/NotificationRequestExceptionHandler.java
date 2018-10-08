package net.ionoff.center.server.notify.connector;

import com.google.gson.Gson;
import net.ionoff.center.server.broker.BrokerResponse;
import net.ionoff.center.server.mediaplayer.exception.MediaPlayerConnectException;
import net.ionoff.center.server.mediaplayer.exception.MediaPlayerRequestException;
import net.ionoff.center.server.notify.connector.exception.NotificationRequestException;
import net.ionoff.center.server.wsclient.RestTemplateExceptionHandler;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

public class NotificationRequestExceptionHandler extends RestTemplateExceptionHandler {

    private final Gson gson = new Gson();

    @Override
    protected RuntimeException buildRestTemplateRequestException(HttpStatus statusCode,
                                                                             HttpHeaders headers, String responseBody) {
        return new NotificationRequestException(String.valueOf(responseBody));
    }

    @Override
    protected RuntimeException newRestTemplateConnectException() {
        return new NotificationRequestException("Error connect notificaton service");
    }
}