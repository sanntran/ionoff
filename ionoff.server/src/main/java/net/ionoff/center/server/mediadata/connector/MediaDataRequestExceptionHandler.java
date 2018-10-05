package net.ionoff.center.server.mediadata.connector;

import com.google.gson.Gson;
import net.ionoff.center.server.broker.BrokerResponse;
import net.ionoff.center.server.mediadata.exception.MediaDataConnectException;
import net.ionoff.center.server.mediadata.exception.MediaDataRequestException;
import net.ionoff.center.server.mediadata.exception.PlayerNotFoundException;
import net.ionoff.center.server.mediadata.model.MediaDataErrorMesage;
import net.ionoff.center.server.wsclient.RestTemplateExceptionHandler;
import net.ionoff.center.server.wsclient.RestTemplateRequestException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;

public class MediaDataRequestExceptionHandler  extends RestTemplateExceptionHandler {

    private final Gson gson = new Gson();

    @Override
    protected RestTemplateRequestException buildRestTemplateRequestException(HttpStatus statusCode,
                                                                             HttpHeaders headers, String responseBody) {

        MediaDataErrorMesage response = gson.fromJson(responseBody, MediaDataErrorMesage.class);
        if ("UnknownPlayerExeption".equals(response.getClazz())) {
            throw new PlayerNotFoundException(String.valueOf(response.getMessage()));
        }
        throw new MediaDataRequestException(String.valueOf(response.getMessage()));
    }

    @Override
    protected RuntimeException newRestTemplateConnectException() {
        return new MediaDataConnectException();
    }
}