package net.ionoff.center.server.wsclient;

import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.SimpleClientHttpRequestFactory;
import org.springframework.web.client.ResponseErrorHandler;
import org.springframework.web.client.RestTemplate;

import java.util.List;

public class RestTemplateFactory {

    public static RestTemplate buildRestTemplate(ResponseErrorHandler errorHandler, List<ClientHttpRequestInterceptor> interceptors) {
        SimpleClientHttpRequestFactory clientHttpRequestFactory = new SimpleClientHttpRequestFactory();
        clientHttpRequestFactory.setConnectTimeout(5000);
        clientHttpRequestFactory.setReadTimeout(10000);
        RestTemplate restClient = new RestTemplate();
        restClient.setRequestFactory(clientHttpRequestFactory);
        restClient.setErrorHandler(errorHandler);
        restClient.setInterceptors(interceptors);

        return restClient;
    }
}
