package net.ionoff.center.server.broker;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

@Component
public class BrokerClientFactory {

    @Value("${broker.http.url}")
    private String brokerUrl;

    public BrokerHttpClient createHttpClient(RestTemplate restTemplate) {
        return new BrokerHttpClient(brokerUrl, restTemplate);
    }
}
