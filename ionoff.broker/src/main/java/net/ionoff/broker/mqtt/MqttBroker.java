package net.ionoff.broker.mqtt;

import io.netty.handler.codec.mqtt.MqttQoS;
import io.vertx.core.Handler;
import io.vertx.mqtt.MqttEndpoint;
import io.vertx.mqtt.MqttServer;
import io.vertx.mqtt.MqttTopicSubscription;

import java.util.ArrayList;
import java.util.List;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.buffer.Buffer;
import net.ionoff.broker.tcp.TcpBroker;

public class MqttBroker extends AbstractVerticle {

    // Convenience method so you can run it in your IDE
    public static void main(String[] args) {
        Runner.runExample(MqttBroker.class);
        new TcpBroker().start();
    }

    private static final List<MqttEndpoint> mqttEndpoints = new ArrayList<>();

    @Override
    public void start() throws Exception {



        MqttServer mqttServer = MqttServer.create(vertx);

        mqttServer
                .exceptionHandler(new Handler<Throwable>() {
                    @Override
                    public void handle(Throwable throwable) {
                        throwable.printStackTrace();
                    }
                });
        mqttServer
                .endpointHandler(endpoint -> {

                    // shows main connect info
                    System.out.println("MQTT client [" + endpoint.clientIdentifier() + "] request to connect, clean session = " + endpoint.isCleanSession());

                    if (endpoint.auth() != null) {
                        System.out.println("[username = " + endpoint.auth().userName() + ", password = " + endpoint.auth().password() + "]");
                    }
                    if (endpoint.will() != null) {
                        System.out.println("[will flag = " + endpoint.will().isWillFlag() + " topic = " + endpoint.will().willTopic() + " msg = " + endpoint.will().willMessage() +
                                " QoS = " + endpoint.will().willQos() + " isRetain = " + endpoint.will().isWillRetain() + "]");
                    }

                    System.out.println("[keep alive timeout = " + endpoint.keepAliveTimeSeconds() + "]");

                    // accept connection from the remote client
                    endpoint.accept(false);

                    // handling requests for subscriptions
                    endpoint.subscribeHandler(subscribe -> {

                        mqttEndpoints.add(endpoint);

                        List<MqttQoS> grantedQosLevels = new ArrayList<>();
                        for (MqttTopicSubscription s : subscribe.topicSubscriptions()) {
                            System.out.println("Subscription for " + s.topicName() + " with QoS " + s.qualityOfService());
                            grantedQosLevels.add(s.qualityOfService());
                        }
                        // ack the subscriptions request
                        endpoint.subscribeAcknowledge(subscribe.messageId(), grantedQosLevels);

                        // just as example, publish a message on the first topic with requested QoS
                        endpoint.publish(subscribe.topicSubscriptions().get(0).topicName(),
                                Buffer.buffer("Hello from the Vert.x MQTT server"),
                                subscribe.topicSubscriptions().get(0).qualityOfService(),
                                false,
                                false);

                        // specifing handlers for handling QoS 1 and 2
                        endpoint.publishAcknowledgeHandler(messageId -> {

                            System.out.println("Received ack for message = " + messageId);

                        }).publishReceivedHandler(messageId -> {
                            System.out.println("Release for message = " + messageId);
                            endpoint.publishRelease(messageId);

                        }).publishCompleteHandler(messageId -> {

                            System.out.println("Received complete for message = " + messageId);
                        });
                    });

                    // handling requests for unsubscriptions
                    endpoint.unsubscribeHandler(unsubscribe -> {

                        for (String t : unsubscribe.topics()) {
                            System.out.println("Unsubscription for " + t);
                        }
                        // ack the subscriptions request
                        endpoint.unsubscribeAcknowledge(unsubscribe.messageId());
                    });

                    // handling ping from client
                    endpoint.pingHandler(v -> {

                        System.out.println("Ping received from client");
                    });

                    // handling disconnect message
                    endpoint.disconnectHandler(v -> {

                        System.out.println("Received disconnect from client");
                    });

                    // handling closing connection
                    endpoint.closeHandler(v -> {

                        System.out.println("Connection closed");
                    });

                    // handling incoming published messages
                    endpoint.publishHandler(message -> {
                        System.out.println("Just received message on [" + message.topicName() + "] payload [" + message.payload() + "] with QoS [" + message.qosLevel() + "]");

                        if (message.qosLevel() == MqttQoS.AT_LEAST_ONCE) {
                            endpoint.publishAcknowledge(message.messageId());
                        } else if (message.qosLevel() == MqttQoS.EXACTLY_ONCE) {
                            endpoint.publishReceived(message.messageId());


                            for (MqttEndpoint ep : mqttEndpoints) {

                                ep.publish("reactivetentando",
                                        Buffer.buffer("reactive microservices client paho"),
                                        MqttQoS.EXACTLY_ONCE,
                                        false, false);
                            }


                        }

                    }).publishReleaseHandler(messageId -> {
                        System.out.println("Publish complete " + messageId);
                        endpoint.publishComplete(messageId);
                    });
                })
                .listen(1883, "0.0.0.0", ar -> {

                    if (ar.succeeded()) {
                        System.out.println("MQTT server is listening on port " + mqttServer.actualPort());
                    } else {
                        System.err.println("Error on starting the server" + ar.cause().getMessage());
                    }
                });
    }
}