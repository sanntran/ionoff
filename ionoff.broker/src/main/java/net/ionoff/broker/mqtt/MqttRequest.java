package net.ionoff.broker.mqtt;

import java.util.concurrent.TimeoutException;

public class MqttRequest {

    private final String keyword;
    private final String subscription;
    private String responseMessage;

    private MqttBroker mqttBroker;

    public MqttRequest(MqttBroker mqttBroker, String subscription, String keyword) {
        this.mqttBroker = mqttBroker;
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
        return false;
    }

    public String sendMqttRequest(String topic, String payload) throws TimeoutException {
        mqttBroker.addPendingRequest(this);
        mqttBroker.publishMessage(topic, payload);
        for (int i = 0; i < 100; i++) { // 10000 milisecond
            try {
                Thread.sleep(100);
                if (responseMessage != null) {
                    return responseMessage;
                }
            } catch (InterruptedException e) {
                // Ignore
            }
        }
        throw new TimeoutException("Timeout MQTT message responseMessage from " + topic);
    }
}
