package net.ionoff.player.connection;

import com.google.gson.Gson;
import net.ionoff.player.config.UserConfig;
import net.ionoff.player.exception.BadRequestException;
import net.ionoff.player.exception.MpdConnectException;
import net.ionoff.player.handler.RequestHandler;
import net.ionoff.player.storage.LocalStorage;
import org.apache.log4j.Logger;
import org.eclipse.paho.client.mqttv3.*;
import org.eclipse.paho.client.mqttv3.persist.MqttDefaultFilePersistence;

import java.util.LinkedHashMap;
import java.util.Map;

public class MqttConnection implements MqttCallback {

	private static final Logger LOGGER = Logger.getLogger(MqttConnection.class.getName());
	private static final Gson GSON = new Gson();


	private String clientId;
	private String status;

	private Thread mqttThread;
	private String brokerUrl;

	private static final String PUBLISH_TOPIC = "MediaPlayer";

	private static final String CONNECTING =   "Connecting...";
	private static final String CONNECTED =    "Connected";
	private static final String DISCONNECTED = "Disconnected";

	private MqttClient client;
	private MqttConnectOptions connOpt;
	private boolean connected = false;
	private String[] subscribleTopics;

	private class StatusPublisher extends Thread {
		@Override
		public void run() {
			try {
				for (; true; ) {
					try {
						sleep(25000);
						publishStatusMessage();
					} catch (Exception e) {
						LOGGER.error(e.getClass().getSimpleName() + " " + e.getMessage(), e);
					}
				}
			}
			catch (Throwable t) {
				LOGGER.error(t.getClass().getSimpleName() + " " + t.getMessage(), t);
			}
		}
	}

	public void start() {
		mqttThread = new Thread() {
			@Override
			public void run() {
				initConnection();
			}
		};
		mqttThread.start();
		new StatusPublisher().start();
	}

	public void initConnection() {
		MqttDefaultFilePersistence filePersistence = new MqttDefaultFilePersistence(LocalStorage.INSTANCE.getDir());
		clientId = UserConfig.getInstance().LICENSE_KEY;
		subscribleTopics = new String[] {clientId};
		brokerUrl = "tcp://" + UserConfig.getInstance().SERVER_HOST + ":" + UserConfig.getInstance().SERVER_PORT;
		try {
			client = new MqttClient(brokerUrl, clientId, filePersistence);
		} catch (MqttException e) {
			LOGGER.error("Error create broker connector " + e.getMessage(), e);
		}
		client.setCallback(this);
		connOpt = new MqttConnectOptions();
		connOpt.setCleanSession(true);

		connectMqttBroker();
	}

	@Override
	public void messageArrived(String topic, MqttMessage message) {
		// Called when a message arrives from the server that matches any
		// subscription made by the connector
		String payload = new String(message.getPayload());
		LOGGER.debug("Message arrived on topic: " + topic + ". Message: " + payload);
		handleMessage(payload);
	}

	private synchronized void handleMessage(String message) {
		String subscription = null;
		Object response;
		try {
			MqttRequestMessage jsonRequest = toMqttRequestMessage(message);
			subscription = jsonRequest.getSubscription();
			if (subscription == null || subscription.isEmpty()) {
				throw new BadRequestException("Request message subscription is empty");
			}
			response = RequestHandler.handleRequest(jsonRequest);
		}
		catch (final Throwable e) {
			if (e instanceof BadRequestException) {
				LOGGER.error("BadRequestException: " + e.getMessage());
			}
			if (e instanceof MpdConnectException) {
				LOGGER.error("MpdConnectException: Error connect MPD ");
			}
			else {
				LOGGER.error(e.getClass().getSimpleName() + ": " + e.getMessage(), e);
			}
			response = new MqttResponseMessage(e.getMessage(), e);
		}
		if (subscription != null && !subscription.isEmpty() && response != null) {
			publishMessage(subscription, GSON.toJson(response));
		}
	}

	private MqttRequestMessage toMqttRequestMessage(String message) {
		try {
			if (message == null) {
				throw new BadRequestException("Request message is null");
			}
			return new MqttRequestMessage(message);
		} catch (BadRequestException e) {
			throw e;
		} catch (Exception e) {
			String msg = e.getClass().getSimpleName() + " error parsing message to json" + e.getMessage();
			LOGGER.error(msg);
			throw new BadRequestException(msg);
		}
	}

	public void publishMessage(String topic, String payload) {
		LOGGER.debug("Publish message to topic: " + topic + ". Message: " + payload);
		if (!isConnected()) {
			LOGGER.error("Error publishing message, disconnected");
			return;
		}
		MqttMessage message = new MqttMessage(payload.getBytes());
		message.setQos(2);
		try {
			client.publish(topic, message);
		} catch (Exception e) {
			LOGGER.error("Error publish message "  + e.getMessage(), e);
		}
	}

	@Override
	public void connectionLost(Throwable cause) {
		status = DISCONNECTED;
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

	private void publishStatusMessage() {
		if (!isConnected()) {
			return;
		}
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("mac", UserConfig.getInstance().LICENSE_KEY);
		map.put("status", RequestHandler.handleStatusRequest());
		String payload = GSON.toJson(map);
		publishMessage(PUBLISH_TOPIC, payload);
	}

	public void connectMqttBroker() {
		try {
			status = CONNECTING;
			LOGGER.info("Connecting to broker " + brokerUrl);
			client.connect(connOpt);
			status = CONNECTED;
			LOGGER.info("Connected to broker " + brokerUrl);
			setConnected(true);
			setSubscribleTopic(subscribleTopics);
		} catch (MqttException e) {
			LOGGER.error("Cannot connect to broker. " + e.getMessage());
			try {
				Thread.sleep(10000);
			} catch (InterruptedException ie) {
				LOGGER.error("InterruptedException: " + ie.getMessage());
			}
			connectMqttBroker();
		}
	}

	private void setConnected(boolean value) {
		this.connected = value;
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
		LOGGER.debug("Delivery message to broker complete. " + token.getMessageId());
	}

	private void setSubscribleTopic(String subscribleTopics[]) {
		try {
			client.unsubscribe(this.subscribleTopics);
		} catch (MqttException e) {
			LOGGER.error(e.getMessage(), e);
		}
		try {
			this.subscribleTopics = subscribleTopics;
			client.subscribe(this.subscribleTopics);
		} catch (MqttException e) {
			LOGGER.error(e.getMessage(), e);
		}
	}

	public boolean isConnected() {
		return connected;
	}

}
