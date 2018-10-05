package net.ionoff.center.server.wsclient;

import org.apache.log4j.Logger;
import org.springframework.http.HttpRequest;
import org.springframework.http.MediaType;
import org.springframework.http.client.ClientHttpRequestExecution;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.http.client.support.HttpRequestWrapper;

import java.io.IOException;
import java.util.Arrays;

public class RestTemplateRequestIntercepter implements ClientHttpRequestInterceptor {

    private static final Logger LOGGER = Logger.getLogger(RestTemplateRequestIntercepter.class.getName());

    @Override
    public ClientHttpResponse intercept(HttpRequest request, byte[] body, ClientHttpRequestExecution execution)
            throws IOException {
        final HttpRequestWrapper requestWrapper = new HttpRequestWrapper(request);
        requestWrapper.getHeaders().setAccept(Arrays.asList(MediaType.APPLICATION_JSON));
        logRequestInformation(request, body);
        ClientHttpResponse clientHttpResponse = execution.execute(requestWrapper, body);
        return clientHttpResponse;
    }


    private void logRequestInformation(HttpRequest request, byte[] body) {
        if (!LOGGER.isDebugEnabled()) {
            return;
        }
        LOGGER.debug(">>>>>>>>>>>>> Http request begin ================================================");
        LOGGER.debug("URI         : " + request.getURI());
        LOGGER.debug("Method      : " +  request.getMethod());
        LOGGER.debug("Headers     : " +  request.getHeaders());
        try {
            LOGGER.debug("Request body: " + new String(body, "UTF-8"));
        } catch (Exception e) {
            LOGGER.error("Error when logging request body: " + e.getMessage());
        }
        LOGGER.debug(">>>>>>>>>>>>> Http request end ==================================================");
    }

}