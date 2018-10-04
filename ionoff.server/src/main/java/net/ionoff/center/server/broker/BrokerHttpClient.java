package net.ionoff.center.server.broker;

import org.springframework.web.client.RestTemplate;

public class BrokerHttpClient {

    private RestTemplate restTemplate;

    public BrokerHttpClient() {
        this.restTemplate = new RestTemplate();
    }


    public String sendCommand(BrokerCommand command) {
        return "";
    }

    public String updateDevice(BrokerDevice device) {
        return "";
    }

    public String setDeviceList(BrokerDeviceList deviceList) {
        return "";
    }
}
