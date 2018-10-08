package net.ionoff.broker.mqtt;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.TimeoutException;

public class MqttRequest {

    private static final Logger LOGGER = LoggerFactory.getLogger(MqttRequest.class);

    private final String topic;
    private final String keyword;
    private final String subscription;
    private String responseMessage;

    private MqttBroker mqttBroker;

    public MqttRequest(MqttBroker mqttBroker, String topic, String subscription, String keyword) {
        this.mqttBroker = mqttBroker;
        this.topic = topic;
        this.subscription = subscription;
        this.keyword = keyword;
    }

    public boolean onMessageArrived(String topic, String message) {
        if (topic.equals(this.subscription)) {
            if (keyword == null || keyword.isEmpty() || message.contains(keyword)) {
                responseMessage = message;
                mqttBroker.removePendingClient(this);
                return true;
            }
        }
        else if (message.startsWith("id=" + this.topic)) {
            if (keyword == null || keyword.isEmpty() || message.contains(keyword)) {
                responseMessage = message;
                mqttBroker.removePendingClient(this);
                return true;
            }
        }
        return false;
    }

    public String sendMqttRequest(String payload) throws TimeoutException {
        mqttBroker.addPendingRequest(this);
        mqttBroker.publishMessage(topic, payload);
        for (int i = 0; i < 60; i++) { // 6 seconds
            try {
                Thread.sleep(100);
                if (responseMessage != null) {
                    return responseMessage;
                }
            } catch (InterruptedException e) {
                LOGGER.error("InterruptedException " + e.getMessage());
            }
        }
        throw new TimeoutException("TimeoutException: timeout reading message response from " + topic);
    }
}
