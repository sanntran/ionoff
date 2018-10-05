package net.ionoff.center.server.broker;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.client.RestTemplate;

public class BrokerClientFactory {

    @Value("${broker.http.url}")
    private String brokerUrl;

    public BrokerHttpClient createHttpClient(RestTemplate restTemplate) {
        return new BrokerHttpClient(brokerUrl, restTemplate);
    }
}
