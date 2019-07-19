package net.xapxinh.player.connection;

import com.google.gson.Gson;
import net.xapxinh.player.AppProperties;
import net.xapxinh.player.config.AppConfig;
import net.xapxinh.player.exception.BadRequestException;
import net.xapxinh.player.exception.MpdConnectException;
import net.xapxinh.player.handler.RequestHandler;
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

	public void initConnection() {
		AppProperties.setPlayerName(AppProperties.getHardwareId());
		MqttDefaultFilePersistence filePersistence = new MqttDefaultFilePersistence(LocalStorage.getAppDir());
		clientId = AppProperties.getHardwareId();
		subscribleTopics = new String[] {clientId};
		brokerUrl = "tcp://" + AppConfig.getInstance().CENTER_SERVER_HOST + ":" + AppConfig.getInstance().CENTER_SERVER_PORT;
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

	public void publishMessage(String topic, String payload) {
		LOGGER.info("Publishing message:" + payload + " to topic: " + topic);
		MqttMessage message = new MqttMessage(payload.getBytes());
		message.setQos(2);
		try {
			client.publish(topic, message);
		} catch (Exception e) {

			LOGGER.error(e.getMessage(), e);
			if (!client.isConnected()) {
				connectMqttBroker();
			}
		}
	}

	@Override
	public void connectionLost(Throwable cause) {
		setConnected(false);
		informStatus(DISCONNECTED);
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

	private synchronized void handleMessage(String message) {
		String subscription = null;
		Object response;
		try {
			MqttRequestMessage jsonRequest = toMqttRequestMessage(message);
			subscription = jsonRequest.getSubscription();
			if (subscription == null || subscription.isEmpty()) {
				throw new BadRequestException("Request message subscription is empty");
			}
			response = new RequestHandler().handleRequest(jsonRequest);
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

	private void informStatus(String status) {
		LOGGER.info("Status: " + status);
		this.status = status;
	}

	public String getStatus() {
		return status;
	}

	public boolean isConnected() {
		return CONNECTED.equals(status);
	}
	
	public void connectMqttBroker() {
		try {
			if (CONNECTING.equals(status)) {
				try {
					Thread.sleep(10000);
				} catch (InterruptedException ie) {
					LOGGER.error("InterruptedException: " + ie.getMessage());
				}
				connectMqttBroker();
				return;
			}
			informStatus(CONNECTING);
			LOGGER.info("Connecting to broker " + brokerUrl);
			client.connect(connOpt);
			informStatus(CONNECTED);
			LOGGER.info("Connected to broker" + brokerUrl);
			setConnected(true);
			publishStatusMessage();
		} catch (MqttException e) {
			LOGGER.error("Cannot connect to broker. " + e.getMessage());
			informStatus(DISCONNECTED);
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
		// delivery of a message will complete after the client has
		// re-connected.
		// The getPendingTokens method will provide tokens for any messages
		// that are still to be delivered.
		LOGGER.info("Delivery message to broker complete. " + token.getMessageId());
	}

	@Override
	public void messageArrived(String topic, MqttMessage message) throws Exception {
		// Called when a message arrives from the server that matches any
		// subscription made by the client
		String payload = new String(message.getPayload());
		LOGGER.info("Message arrived on topic: " + topic + ". Message: " + payload);
		handleMessage(payload);
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

	private void publishStatusMessage() {
		if (!isConnected()) {
			return;
		}
		Map<String, Object> map = new LinkedHashMap<>();
		map.put("mac", AppProperties.getHardwareId());
		map.put("status", new RequestHandler().handleStatusRequest());
		String payload = GSON.toJson(map);
		publishMessage(PUBLISH_TOPIC, payload);
	}
}
