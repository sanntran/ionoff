package net.ionoff.center.server.broker;

import org.springframework.web.client.RestTemplate;

public class BrokerHttpClient {

    private RestTemplate restTemplate;

    private String brokerUrl;

    public BrokerHttpClient(String brokerUrl, RestTemplate restTemplate) {
        this.brokerUrl = brokerUrl;
        this.restTemplate = restTemplate;
    }

    public BrokerResponse sendCommand(BrokerCommand command) {
        return restTemplate.postForObject(brokerUrl + "/commands", command, BrokerResponse.class);
    }

    public String updateDevice(BrokerDevice device) {
        return "";
    }

    public String setDeviceList(BrokerDeviceList deviceList) {
        return "";
    }
}
