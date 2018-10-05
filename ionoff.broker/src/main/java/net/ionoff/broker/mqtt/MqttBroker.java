package net.ionoff.broker.mqtt;

import io.netty.handler.codec.mqtt.MqttQoS;
import io.vertx.mqtt.MqttEndpoint;
import io.vertx.mqtt.MqttServer;
import io.vertx.mqtt.MqttTopicSubscription;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.buffer.Buffer;
import io.vertx.mqtt.messages.MqttUnsubscribeMessage;
import net.ionoff.broker.AppProperties;
import net.ionoff.broker.tcp.TcpBroker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MqttBroker extends AbstractVerticle {

    private static final Logger LOGGER = LoggerFactory.getLogger(MqttBroker.class);

    /**
     * Topics that are allowed to publish and subscribe.
     */
    private final List<String> PUBLISH_TOPIC_LIST = AppProperties.getBrokerTopics();

    private final List<MqttRequest> PENDING_MQTT_REQUESTS = Collections.synchronizedList(new ArrayList<>());

    private final Map<String, List<MqttEndpoint>> MQTT_ENDPOINTS = new ConcurrentHashMap<>();

    @Override
    public void start() {

        TcpBroker tcpBroker = new TcpBroker(this);
        tcpBroker.start();

        MqttServer mqttServer = MqttServer.create(vertx);

        mqttServer.exceptionHandler(t -> LOGGER.error(t.getMessage(), t));
        mqttServer.endpointHandler(endpoint -> {
            LOGGER.info("MQTT connector [" + endpoint.clientIdentifier() + "] request to connect, clean session = "
                    + endpoint.isCleanSession());
            if (endpoint.auth() != null) {
                LOGGER.info("[username = " + endpoint.auth().userName() + ", password = "
                        + endpoint.auth().password() + "]");
            }
            if (endpoint.will() != null) {
                LOGGER.info("[will flag = " + endpoint.will().isWillFlag() + " topic = "
                        + endpoint.will().willTopic() + " msg = " + endpoint.will().willMessage() +
                        " QoS = " + endpoint.will().willQos() + " isRetain = " + endpoint.will().isWillRetain() + "]");
            }
            LOGGER.info("[keep alive timeout = " + endpoint.keepAliveTimeSeconds() + "]");
            endpoint.accept(true);
            endpoint.subscribeHandler(subscribe -> {
                List<MqttQoS> grantedQosLevels = new ArrayList<>();
                for (MqttTopicSubscription topicSubscription : subscribe.topicSubscriptions()) {
                    LOGGER.info("Subscription for " + topicSubscription.topicName() + " with QoS " + topicSubscription.qualityOfService());
                    grantedQosLevels.add(topicSubscription.qualityOfService());
                    onEnpointSubscribed(topicSubscription, endpoint);
                }
                endpoint.subscribeAcknowledge(subscribe.messageId(), grantedQosLevels);
                // specifing handlers for handling QoS 1 and 2
                endpoint.publishAcknowledgeHandler(messageId -> {
                    LOGGER.info("Received ack for message = " + messageId);
                }).publishReceivedHandler(messageId -> {
                    LOGGER.info("Release for message = " + messageId);
                    endpoint.publishRelease(messageId);
                }).publishCompleteHandler(messageId -> {
                    LOGGER.info("Received complete for message = " + messageId);
                });
            });
            endpoint.unsubscribeHandler(unsubscribe -> {
                endpoint.unsubscribeAcknowledge(unsubscribe.messageId());
                onEnpointUnsubscribed(unsubscribe, endpoint);
            });
            endpoint.pingHandler(v -> {
                // LOGGER.info("Ping received from connector");
            });
            endpoint.disconnectHandler(v -> {
                LOGGER.info("Received disconnect from connector");
                onEnpointDisconnected(endpoint);
            });
            endpoint.closeHandler(v -> {
                LOGGER.info("Connection closed");
            });
            endpoint.publishHandler(message -> {
                LOGGER.info("Received message on [" + message.topicName() + "] payload ["
                        + message.payload() + "] with QoS [" + message.qosLevel() + "]");
                if (message.qosLevel() == MqttQoS.AT_LEAST_ONCE) {
                    endpoint.publishAcknowledge(message.messageId());
                } else if (message.qosLevel() == MqttQoS.EXACTLY_ONCE) {
                    endpoint.publishReceived(message.messageId());
                }
                boolean consume = false;
                for (int i = 0; i < PENDING_MQTT_REQUESTS.size(); i++) {
                    if (PENDING_MQTT_REQUESTS.get(i).onMessageArrived(message.topicName(), message.payload().toString())) {
                        consume = true;
                        break;
                    }
                }
                if (consume == false && PUBLISH_TOPIC_LIST.contains(message.topicName())) {
                    // Skip publish to subscriber on this topic
                    publishMessage(message.topicName(), message.payload().toString());
                }
            }).publishReleaseHandler(messageId -> {
                LOGGER.info("Publish complete " + messageId);
                endpoint.publishComplete(messageId);
            });
        })
        .listen(1883, "0.0.0.0", ar -> {
            if (ar.succeeded()) {
                LOGGER.info("MQTT server is listening on port " + mqttServer.actualPort());
            } else {
                LOGGER.info("Error on starting the server" + ar.cause().getMessage());
            }
        });
    }

    public void publishMessage(String topic, String data) {
        List<MqttEndpoint> endpoints = MQTT_ENDPOINTS.get(topic);
        if (endpoints != null) {
            for (MqttEndpoint endpoint : endpoints) {
                if (endpoint.isConnected()) {
                    endpoint.publish(topic, Buffer.buffer(data), MqttQoS.EXACTLY_ONCE, false, false);
                }
            }
        }
    }

    private void onEnpointUnsubscribed(MqttUnsubscribeMessage unsubscribe, MqttEndpoint endpoint) {
        for (String topic : unsubscribe.topics()) {
            List<MqttEndpoint> endpoints = MQTT_ENDPOINTS.get(topic);
            if (endpoints != null) {
                for (int i = 0; i < endpoints.size(); i++) {
                    if (endpoint.equals(endpoints.get(i))) {
                        endpoints.remove(i);
                        i --;
                    }
                }
            }
        }
    }

    private void onEnpointDisconnected(MqttEndpoint endpoint) {
        for (Map.Entry<String, List<MqttEndpoint>> entry : MQTT_ENDPOINTS.entrySet()) {
            List<MqttEndpoint> endpoints = entry.getValue();
            if (endpoints != null) {
                for (int i = 0; i < endpoints.size(); i++) {
                    if (endpoint.equals(endpoints.get(i))) {
                        endpoints.remove(i);
                        i --;
                    }
                }
            }
        }
    }

    private void onEnpointSubscribed(MqttTopicSubscription topicSubscription, MqttEndpoint endpoint) {
        List<MqttEndpoint> endpoints = MQTT_ENDPOINTS.get(topicSubscription.topicName());
        if (endpoints == null) {
            endpoints = Collections.synchronizedList(new ArrayList<>());
            MQTT_ENDPOINTS.put(topicSubscription.topicName(), endpoints);
        }
        MQTT_ENDPOINTS.get(topicSubscription.topicName()).add(endpoint);
    }

    public void addPendingRequest(MqttRequest syncRequest) {
        PENDING_MQTT_REQUESTS.add(syncRequest);
    }

    public void removePendingClient(MqttRequest syncRequest) {
        PENDING_MQTT_REQUESTS.remove(syncRequest);
    }
}