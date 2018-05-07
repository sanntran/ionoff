package net.ionoff.center.server.thread;


import java.util.Date;
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
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.WeighScale;
import net.ionoff.center.server.exception.MqttConnectionException;
import net.ionoff.center.server.exception.MqttPublishException;
import net.ionoff.center.server.persistence.service.IRelayDriverService;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.persistence.service.ISensorService;


public class MosquittoClient implements MqttCallback {
	
	private static Logger LOGGER = Logger.getLogger(MosquittoClient.class.getName());
	
	private MqttClient client;
	private MqttConnectOptions connOpt;
	private boolean connected = false;
	private String[] subscribleTopics;
	
	@Autowired
	private ServerThreadPool threadPull; 

	@Autowired
	private IDeviceService deviceService;
	
	@Autowired
	private IRelayDriverService relayDriverService;
	
	@Autowired
	private ISensorService sensorService;
	
	@Autowired
	private RelayDriverStatusHandler relayDriverStatusHandler;
	
	public MosquittoClient() {
		new Thread() {
			@Override
			public void run() {
				init();
			}
		}.start();
	}
	
	public void init() {
		subscribleTopics = new String[] {AppConfig.getInstance().MQTT_TOPIC_IONOFF_NET, 
				AppConfig.getInstance().MQTT_TOPIC_RELAY_DRIVER, AppConfig.getInstance().MQTT_TOPIC_WEIGH_SCALE};
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
		
		if (AppConfig.getInstance().MQTT_TOPIC_IONOFF_NET.equals(topic) ||
				AppConfig.getInstance().MQTT_TOPIC_RELAY_DRIVER.equals(topic)) {
			onRelayDriverDriverMessageArrived(payload);
		}
		else if (AppConfig.getInstance().MQTT_TOPIC_WEIGH_SCALE.equals(topic)) {
			onWeighScaleMessageArrived(payload);
		}
	}
	
	private void onWeighScaleMessageArrived(String payload) {
		LOGGER.info("Message arrived: " + payload);
		WeighScaleMqttPayload data = new WeighScaleMqttPayload(payload);
		WeighScale scale = deviceService.findWeighScaleByMac(data.getId());
		if (scale == null) {
			LOGGER.info("Found no weigh scale by id: " + data.getId());
			return;
		}
		Date now = new Date();
		scale.setTime(now);
		if (scale.getSensors() == null || scale.getSensors().isEmpty()) {
			deviceService.update(scale);
			return;
		}

		if (data.getValue() == null || data.getIndex() == null) {
			LOGGER.info("Invalid message format");
			publishMessage(scale.getMac(), "InvalidMessage: " + payload);
			deviceService.update(scale);
			return;
		}

		Sensor sensor = scale.getSensors().get(0);
		sensor.getStatus().setTime(now);
		sensor.getStatus().setValue(data.getValue());
		sensor.getStatus().setIndex(data.getIndex());
		sensorService.update(sensor);
		sensorService.updateStatus(sensor.getStatus());

		if (WeighScaleMqttPayload.CHANGED.equals(data.getCode())) {
			sensorService.insertSensorData(sensor.getStatus());
			publishMessage(scale.getMac(), "MessageIndexOK: " + data.getIndex());
		}
	}

	private void onRelayDriverDriverMessageArrived(String payload) {
		RelayDriverMqttPayload mqttPayload = new RelayDriverMqttPayload(payload);
		if (mqttPayload.getId() == null) {
			LOGGER.info("Message is not valid format " + payload);
			return;
		}
		List<RelayDriver> relayDrivers = relayDriverService.findByMac(mqttPayload.getId());
		if (relayDrivers.isEmpty()) {
			LOGGER.info("No relayDriver found in the DB. Key: " + mqttPayload.getId());
			return;
		}
		
		RelayDriver relayDriver = relayDrivers.get(0);
		relayDriver.setConnectedTime(System.currentTimeMillis());
		relayDriverService.update(relayDriver);
		if (!relayDriver.isConnected()) {
			LOGGER.info("RelayDriver " + relayDriver.getKey() + " is now connected");
		}
		if (RelayDriverMqttPayload.STATUS.equals(mqttPayload.getCode())) {
			//LOGGER.info("RelayDriver " + relayDriver.getKey() + " has been connected");
			relayDriverStatusHandler.onReceivedRelayDriverStatus(relayDriver, mqttPayload.getIn(),  mqttPayload.getOut());
		} else if (RelayDriverMqttPayload.CHANGED.equals(mqttPayload.getCode())) {
			LOGGER.info("RelayDriver " + relayDriver.getKey() + " input status has been changed");
			relayDriverStatusHandler.onRelayDriverStatusChanged(relayDriver, mqttPayload.getIn(),  mqttPayload.getOut());
		} else if (RelayDriverMqttPayload.RESET.equals(mqttPayload.getCode())) {
			LOGGER.info("RelayDriver " + relayDriver.getKey() + " has been started");
			relayDriverStatusHandler.onRelayDriverStarted(relayDriver, null, mqttPayload.getIn(),  mqttPayload.getOut());
		} else if (RelayDriverMqttPayload.CRASH.equals(mqttPayload.getCode())) {
			LOGGER.info("RelayDriver " + relayDriver.getKey() + " started due to crash");
			relayDriverStatusHandler.onRelayDriverCrashed(relayDriver, mqttPayload.getIn(),  mqttPayload.getOut());
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
	
}
