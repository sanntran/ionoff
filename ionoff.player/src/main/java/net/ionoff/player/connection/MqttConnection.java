package net.ionoff.player.connection;

import net.ionoff.player.config.UserConfig;
import net.ionoff.player.exception.BadRequestException;
import net.ionoff.player.exception.MpdConnectException;
import net.ionoff.player.handler.RequestHandler;
import net.ionoff.player.storage.LocalStorage;
import org.apache.log4j.Logger;
import org.eclipse.paho.client.mqttv3.*;
import org.eclipse.paho.client.mqttv3.persist.MqttDefaultFilePersistence;
import org.json.JSONException;
import org.json.JSONObject;

import java.net.SocketException;
import java.util.IllegalFormatException;

public class MqttConnection implements MqttCallback {

	private static final Logger LOGGER = Logger.getLogger(MqttConnection.class.getName());

	private String clientId;
	private String status;
	private PublishMessageSchedule publishMessageSchedule;

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

	public void initConnection() {
		MqttDefaultFilePersistence filePersistence = new MqttDefaultFilePersistence(LocalStorage.INSTANCE.getDir());
		clientId = UserConfig.getInstance().LICENSE_KEY;
		subscribleTopics = new String[] {};
		brokerUrl = "tcp://" + UserConfig.getInstance().SERVER_HOST + ":" + UserConfig.getInstance().SERVER_PORT;
		try {
			client = new MqttClient(brokerUrl, clientId, filePersistence);
		} catch (MqttException e) {
			LOGGER.error("Error create mqtt client " + e.getMessage(), e);
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
			LOGGER.error("Error publish message "  + e.getMessage(), e);
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

	public void run() {
		publishMessageSchedule = new PublishMessageSchedule();
		publishMessageSchedule.start();
	}

	private synchronized String handleMesage(String message) {
		try {
			if (message == null) {
				throw new SocketException("Request is null");
			}
			final RequestMessage request = new RequestMessage(message);
			if (request.getParameters() == null) {
				throw new BadRequestException("Request parametter is null, message: " + message);
			}
			return new RequestHandler(request).handleRequest();
		}
		catch (final Throwable e) {
			if (e instanceof BadRequestException) {
				LOGGER.error(e.getMessage());
			}
			if (e instanceof MpdConnectException) {
				LOGGER.error("Error connect MPD ");
			}
			else {
				LOGGER.error("Error handle message " + e.getMessage(), e);
			}
			return new ResponseMessage(e.getMessage(), e).toJSONString();
		}
	}

	public void connect() {
		try {



			final JSONObject param = new JSONObject();

			param.put("mac", AppProperties.getHardwareId());
			param.put("pass", AppProperties.getRandomPassword());
			param.put("password", UserConfig.getInstance().LOGIN_PASSWORD);

			publishMessage(PUBLISH_TOPIC, param.toString());

		}
		catch (final Throwable e) {
			try {
				Thread.sleep(10000);
			}
			catch (final InterruptedException ie) {
				LOGGER.error(ie);
			}
			LOGGER.error(e);
			connect();
		}
	}

	private void onRecievedResponse(String received) {
		LOGGER.info("Response: " + received);
		try {
			final JSONObject info = new JSONObject(received);

		}
		catch (final JSONException e) {
			LOGGER.error(e.getMessage(), e);
		}
	}

	private void informStatus(String status) {
		LOGGER.info("Status: " + status);
		this.status = status;
	}

	public String getStatus() {
		return status;
	}

	public boolean isConected() {
		return CONNECTED.equals(status);
	}
	
	private class PublishMessageSchedule extends Thread {
		
		@Override
		public void run() {
			try {
				for (; true; ) {
					try {
						sleep(5000);
					} catch (Exception e) {
						LOGGER.error(e.getMessage(), e);
					}
				}
			}
			catch (Throwable t) {
				LOGGER.error(t.getMessage(), t);
			}
		}
	}


	public void connectMqttBroker() {
		try {
			informStatus(CONNECTING);
			LOGGER.info("Connecting to broker " + brokerUrl);
			client.connect(connOpt);
			informStatus(CONNECTED);
			LOGGER.info("Connected to broker" + brokerUrl);
			setConnected(true);
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
		handleMesage(payload);
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

	public void start() {
		mqttThread = new Thread() {
			@Override
			public void run() {
				initConnection();
			}
		};
		mqttThread.start();
	}

}
