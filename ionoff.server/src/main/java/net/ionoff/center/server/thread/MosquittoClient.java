package net.ionoff.center.server.thread;


import java.util.List;

import org.apache.log4j.Logger;
import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;
import org.eclipse.paho.client.mqttv3.MqttCallback;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.config.AppConfig;
import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.exception.MqttConnectionException;
import net.ionoff.center.server.exception.MqttPublishException;
import net.ionoff.center.server.persistence.service.IControllerService;


public class MosquittoClient implements MqttCallback {
	
	private static Logger LOGGER = Logger.getLogger(MosquittoClient.class.getName());
	
	private MqttClient client;
	private MqttConnectOptions connOpt;
	private boolean connected = false;
	private String subscribleTopic;
	private long messageArrivedTime = 0;
	
	@Autowired
	private ServerThreadPool threadPull; 
	
	@Autowired
	private IControllerService controllerService;
	
	@Autowired
	private ControllerStatusHandler controllerStatusHandler;
	
	public MosquittoClient() {
		new Thread() {
			@Override
			public void run() {
				init();
			}
		}.start();
	}
	
	public void init() {
		try {
			client = new MqttClient(AppConfig.getInstance().MQTT_BROKER_URL
					, AppConfig.getInstance().MQTT_CLIENT_ID + System.currentTimeMillis());
		} catch (MqttException e) {
			LOGGER.error(e.getMessage(), e);
		}
		client.setCallback(this);
		connOpt = new MqttConnectOptions();
		connOpt.setCleanSession(true);
		//connOpt.setKeepAliveInterval(60);
		connOpt.setUserName(AppConfig.getInstance().MQTT_USER);
		connOpt.setPassword(AppConfig.getInstance().MQTT_PASS.toCharArray());
		
		connectBroker();
	}

	public void publishMessage(String topic, String payload) {
		
		if (!connected) {
			throw new MqttConnectionException("Not connected to MQTT server: " + AppConfig.getInstance().MQTT_BROKER_URL);
		}
		
		MqttMessage message = new MqttMessage(payload.getBytes());
        message.setQos(AppConfig.getInstance().MQTT_QOS);
        LOGGER.info("Publishing message:" + message + " to topic: " + topic);
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
			LOGGER.error(e.getMessage(), e);
		}
		new Thread() {
			@Override
			public void run() {
				if (threadPull.isShutdown()) {
					return;
				}
				connectBroker();
			}
		}.start();
	}

	public void connectBroker() {
		try {
			LOGGER.info("Connecting to broker " + AppConfig.getInstance().MQTT_BROKER_URL);
			client.connect(connOpt);
			LOGGER.info("Connected to broker" + AppConfig.getInstance().MQTT_BROKER_URL);
			setConnected(true);
		} catch (MqttException e) {
			LOGGER.error("Cannot connect to broker. " + e.getMessage());
			try {
				Thread.sleep(15000);
			} catch (InterruptedException ie) {
				LOGGER.error(ie.getMessage(), ie);
			}
			connectBroker();
		}
	}
	
	private void setConnected(boolean value) {
		this.connected = value;
		if (connected) {
			setSubscribleTopic(AppConfig.getInstance().MQTT_TOPIC);
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
		messageArrivedTime = System.currentTimeMillis();
		// Called when a message arrives from the server that matches any
		// subscription made by the client
		String payload = new String(message.getPayload());
		LOGGER.info("Message arrived: " + payload);
		
		if (AppConfig.getInstance().MQTT_TOPIC.equals(topic)) {
			onMessageArrived(payload);
		}
	}
	
	private void onMessageArrived(String payload) {
		MqttPayload mqttPayload = new MqttPayload(payload);
		if (mqttPayload.getId() == null) {
			LOGGER.info("Message is not valid format " + payload);
			return;
		}
		List<Controller> controllers = controllerService.findByMac(mqttPayload.getId());
		if (controllers.isEmpty()) {
			LOGGER.info("No controller found in the DB. Key: " + mqttPayload.getId());
			return;
		}
		
		Controller controller = controllers.get(0);
		controller.setConnectedTime(System.currentTimeMillis());
		controllerService.update(controller);
		if (!controller.isConnected()) {
			LOGGER.info("Controller " + controller.getKey() + " is now connected");
		}
		if (MqttPayload.STATUS.equals(mqttPayload.getCode())) {
			LOGGER.info("Controller " + controller.getKey() + " has been connected");
			controllerStatusHandler.onReceivedControllerStatus(controller, mqttPayload.getIn(),  mqttPayload.getOut());
		} else if (MqttPayload.CHANGED.equals(mqttPayload.getCode())) {
			LOGGER.info("Controller " + controller.getKey() + " input status has been changed");
			controllerStatusHandler.onControllerStatusChanged(controller, mqttPayload.getIn(),  mqttPayload.getOut());
		} else if (MqttPayload.RESET.equals(mqttPayload.getCode())) {
			LOGGER.info("Controller " + controller.getKey() + " has been started");
			controllerStatusHandler.onControllerStarted(controller, null, mqttPayload.getIn(),  mqttPayload.getOut());
		} else if (MqttPayload.CRASH.equals(mqttPayload.getCode())) {
			LOGGER.info("Controller " + controller.getKey() + " started due to crash");
			controllerStatusHandler.onControllerCrashed(controller, mqttPayload.getIn(),  mqttPayload.getOut());
		}
	}

	public void setSubscribleTopic(String subscribleTopic) {
		if (this.subscribleTopic != null) {
			try {
				client.unsubscribe(this.subscribleTopic);
			} catch (MqttException e) {
				LOGGER.error(e.getMessage(), e);
			}
		}
		this.subscribleTopic = subscribleTopic;
		try {
			client.subscribe(this.subscribleTopic);
		} catch (MqttException e) {
			LOGGER.error(e.getMessage(), e);
		}
	}
	
	public boolean isThingOnline() {
		return System.currentTimeMillis() - messageArrivedTime < 60000;
	}

	public boolean isConnected() {
		return connected;
	}
	
}
