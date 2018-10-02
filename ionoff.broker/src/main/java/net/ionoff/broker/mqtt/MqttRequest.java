package net.ionoff.broker.mqtt;

import java.util.UUID;
import java.util.concurrent.TimeoutException;

public class MqttRequest {

    private String uuid;
    private String topic;
    private String response;
    private MqttBroker mqttBroker;

    public MqttRequest(MqttBroker mqttBroker) {
        uuid = UUID.randomUUID().toString();
        this.mqttBroker = mqttBroker;
    }

    public boolean onMessageArrived(String topic, String message) {
        if (this.topic.equals(topic)) {
            if (message.startsWith("id=" + this.topic)) {
                response = message;
                mqttBroker.removePendingClient(this);
                return true;
            }
        }
        return false;
    }

    public String sendMqttRequest(String topic, String payload) throws TimeoutException {
        this.topic = "Response/" + topic;
        mqttBroker.addPendingRequest(this);
        mqttBroker.publishMessage(topic, payload);
        for (int i = 0; i < 100; i++) { // 10000 milisecond
            try {
                Thread.sleep(100);
                if (response != null) {
                    return response;
                }
            } catch (InterruptedException e) {
                // Ignore
            }
        }
        throw new TimeoutException("Timeout MQTT message response from " + topic);
    }
}
