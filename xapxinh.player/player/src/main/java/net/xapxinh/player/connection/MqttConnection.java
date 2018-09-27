package net.xapxinh.player.connection;

import net.xapxinh.player.AppProperties;
import net.xapxinh.player.config.AppConfig;
import net.xapxinh.player.config.UserConfig;
import net.xapxinh.player.handler.PlayerResponse;
import net.xapxinh.player.handler.RequestHandler;
import net.xapxinh.player.server.exception.PlayerException;
import org.apache.log4j.Logger;
import org.eclipse.paho.client.mqttv3.*;
import org.json.JSONException;
import org.json.JSONObject;

import java.net.SocketException;

public class MqttConnection implements MqttCallback {

	private static final Logger LOGGER = Logger.getLogger(MqttConnection.class.getName());

	private String clientId;
	private String status;
	private IntervalUpdateThread intervalUpdate;

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

	public void initAndConnectBroker() {
		AppProperties.setPlayerName(AppProperties.getHardwareId());
		clientId = "-" + hashCode() + "";
		subscribleTopics = new String[] {};
		brokerUrl = "tcp://" + AppConfig.getInstance().CENTER_SERVER_HOST + ":" + AppConfig.getInstance().CENTER_SERVER_PORT;
		try {
			client = new MqttClient(brokerUrl, clientId);
		} catch (MqttException e) {
			LOGGER.error(e.getMessage(), e);
		}
		client.setCallback(this);
		connOpt = new MqttConnectOptions();
		connOpt.setCleanSession(true);
		//connOpt.setKeepAliveInterval(60);
		//connOpt.setUserName(user);
		//connOpt.setPassword(password.toCharArray());

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
		intervalUpdate = new IntervalUpdateThread();
		intervalUpdate.start();
	}



	private synchronized String handleTcpRequest(String request) throws SocketException {
		try {
			if (request == null) {
				throw new SocketException("Request is null");
			}
			final TcpRequest tcpRequest = new TcpRequest(request);
			if (tcpRequest.getParameters() == null) {
				throw new SocketException("Request parametter is null");
			}
			return new RequestHandler(tcpRequest).handleRequest();
		}
		catch (final Throwable e) {
			if (e instanceof PlayerException) {
				LOGGER.error(e.getMessage());
			}
			else {
				LOGGER.error(e.getMessage(), e);
				throw new SocketException();
			}
			return new PlayerResponse(e.getMessage(), e).toJSONString();
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
	
	private class IntervalUpdateThread extends Thread {
		
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
				initAndConnectBroker();
			}
		};
		mqttThread.start();
	}
}
