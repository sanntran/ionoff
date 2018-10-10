package net.ionoff.center.server.wsclient;

import com.google.common.base.Charsets;
import com.google.common.io.ByteStreams;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.web.client.DefaultResponseErrorHandler;
import org.springframework.web.client.ResponseErrorHandler;

import java.io.IOException;

public abstract class RestTemplateExceptionHandler implements ResponseErrorHandler {

    private static final Logger LOGGER = LoggerFactory.getLogger(RestTemplateExceptionHandler.class.getName());

    private final ResponseErrorHandler errorHandler = new DefaultResponseErrorHandler();

    @Override
    public boolean hasError(ClientHttpResponse response) throws IOException {
        return errorHandler.hasError(response);
    }

    @Override
    public void handleError(ClientHttpResponse response) throws IOException {
        final int statusCode = response.getStatusCode().value();
        if (statusCode == 0) {
            throw newRestTemplateConnectException();
        }
        final String responseBody = new String(ByteStreams.toByteArray(response.getBody()), Charsets.UTF_8);
        logHttpResponseError(response.getStatusCode(), response.getHeaders(), responseBody);
        throw buildRestTemplateRequestException(response.getStatusCode(), response.getHeaders(), responseBody);
    }

    protected RuntimeException newRestTemplateConnectException() {
        return new RestTemplateConnectException();
    }

    protected void logHttpResponseError(HttpStatus statusCode, HttpHeaders headers, String responseBody) {
        if (!LOGGER.isDebugEnabled()) {
            return;
        }
        LOGGER.debug(">>>>>>>>>>>>> Http response error ================================================");
        LOGGER.debug("HttpStatus  : " + statusCode.value());
        LOGGER.debug("HttpHeaders : " +  headers);
        LOGGER.debug("ResponseBody: " +  responseBody);
        LOGGER.debug(">>>>>>>>>>>>> Http response error ==================================================");
    }

    protected abstract RuntimeException buildRestTemplateRequestException(
                                        HttpStatus statusCode, HttpHeaders headers, String responseBody);
}