package net.ionoff.center.server.broker;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;
import org.eclipse.paho.client.mqttv3.MqttCallback;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import net.ionoff.center.server.exception.MqttConnectionException;
import net.ionoff.center.server.exception.MqttPublishException;
import net.ionoff.center.server.mediaplayer.MediaPlayerHandler;
import net.ionoff.center.server.controller.ControllerHandler;
import org.springframework.stereotype.Component;


@Component
public class MqttConnection implements MqttCallback {
	
	private static Logger LOGGER = LoggerFactory.getLogger(MqttConnection.class.getName());

	private boolean shutdown;
	private MqttClient client;
	private MqttConnectOptions connOpt;
	private boolean connected = false;
	private String[] subscribleTopics;

	private Thread mosquittoThread;

	@Value("${broker.mqtt.user}")
	private String user;

	@Value("${broker.mqtt.pass}")
	private String password;

	@Value("${broker.mqtt.qos:2}")
	private Integer qos;

	private String clientId;

	@Value("${broker.mqtt.url}")
	private String brockerUrl;

	@Value("${broker.mqtt.topic.ionoffnet}")
	private String defaultTopic;

	@Value("${broker.mqtt.topic.controller}")
	private String topicController;

	@Value("${broker.mqtt.topic.mediaplayer}")
	private String topicMediaPlayer;

	@Autowired
	private ControllerHandler controllerHandler;

	@Autowired
	private MediaPlayerHandler mediaPlayerHandler;

	public void initAndConnectBroker() {
		clientId = "ionoff-" + hashCode() + "";

		subscribleTopics = new String[] {defaultTopic, topicController, topicMediaPlayer};
		try {
			client = new MqttClient(brockerUrl, clientId);
		} catch (MqttException e) {
			LOGGER.error(e.getMessage(), e);
		}
		client.setCallback(this);
		connOpt = new MqttConnectOptions();
		connOpt.setCleanSession(true);
		//connOpt.setKeepAliveInterval(60);
		connOpt.setUserName(user);
		connOpt.setPassword(password.toCharArray());
		
		connectMqttBroker();
	}

	public void publishMessage(String topic, String payload) {
		
		if (!connected) {
			throw new MqttConnectionException("Not connected to MQTT server: " + brockerUrl);
		}
		LOGGER.info("Publishing message:" + payload + " to topic: " + topic);
		MqttMessage message = new MqttMessage(payload.getBytes());
        message.setQos(qos);
        
        try {
			client.publish(topic, message);
		} catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
			throw new MqttPublishException(e.getMessage());
		}
	}
	
	@Override
	public void connectionLost(Throwable cause) {
		setConnected(false);
		// Called when the connection to the server has been lost.
		// An application may choose to implement reconnection logic at this
		// point.
		LOGGER.error("Mqtt connection lost! " + cause.getMessage(), cause);
		try {
			Thread.sleep(5000);
		} catch (InterruptedException e) {
			LOGGER.error("InterruptedException: " + e.getMessage());
		}
		connectMqttBroker();
	}

	public void connectMqttBroker() {
		if (shutdown == true) {
			return;
		}
		try {
			LOGGER.info("Connecting to broker " + brockerUrl);
			client.connect(connOpt);
			LOGGER.info("Connected to broker " + brockerUrl);
			setConnected(true);
		} catch (MqttException e) {
			LOGGER.error("Cannot connect to broker. " + e.getMessage());
			try {
				Thread.sleep(15000);
			} catch (InterruptedException ie) {
				LOGGER.error("InterruptedException: " + ie.getMessage());
			}
			connectMqttBroker();
		}
	}
	
	private void setConnected(boolean value) {
		this.connected = value;
		if (connected) {
			setSubscribleTopic(subscribleTopics);
		}
	}

	@Override
	public void deliveryComplete(IMqttDeliveryToken token) {
		// Called when a message has been delivered to the
		// server. The token passed in here is the same one
		// that was passed to or returned from the original call to publish.
		// This allows applications to perform asynchronous
		// delivery without blocking until delivery completes.
		//
		// If the connection to the server breaks before delivery has completed
		// delivery of a message will complete after the connector has
		// re-connected.
		// The getPendingTokens method will provide tokens for any messages
		// that are still to be delivered.
		LOGGER.info("Delivery message to broker complete. " + token.getMessageId());
	}
	
	@Override
	public void messageArrived(String topic, MqttMessage message) {
		String payload = new String(message.getPayload());
		LOGGER.debug("Message arrived on topic: " + topic + ". Message: " + payload);
		try {
			// Called when a message arrives from the server that matches any
			// subscription made by the connector
			if (defaultTopic.equals(topic) || topicController.equals(topic)) {
				controllerHandler.onMessageArrived(payload);
			}
			else if (topicMediaPlayer.equals(topic)) {
				mediaPlayerHandler.onMessageArrived(payload);
			}
		} catch (Exception e) {
			LOGGER.error("Error handle MQTT message {" + payload + "} " + e.getMessage(), e);
		}

	}

	private void setSubscribleTopic(String subscribleTopic[]) {
		try {
			client.unsubscribe(subscribleTopics);
		} catch (MqttException e) {
			LOGGER.error(e.getMessage(), e);
		}
		try {
			client.subscribe(subscribleTopics);
		} catch (MqttException e) {
			LOGGER.error(e.getMessage(), e);
		}
	}

	public boolean isConnected() {
		return connected;
	}

	public void shutdown() {
		shutdown = true;
		if (mosquittoThread != null) {
			mosquittoThread.interrupt();
		}
	}

	public void start() {
		mosquittoThread = new Thread() {
			@Override
			public void run() {
				initAndConnectBroker();
			}
		};
		mosquittoThread.start();
	}
	
}
