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
        if (MqttBroker.TOPIC_IONOFF.equals(topic) ||
                MqttBroker.TOPIC_RELAYDRIVER.equals(topic) ||
                MqttBroker.TOPIC_SENSORDRIVER.equals(topic)) {
            if (message.startsWith("id=" + this.topic)) {
                response = message;
                mqttBroker.removePendingClient(this);
                return true;
            }

        }
        else if (MqttBroker.TOPIC_MEDIAPLAYER.equals(topic)) {
            if (message.contains("\"uuid\":\"" + uuid + "\"")) {
                response = message;
                mqttBroker.removePendingClient(this);
                return true;
            }
        }
        return false;
    }

    public String sendMqttRequest(String topic, String payload) throws TimeoutException {
        this.topic = topic;
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
