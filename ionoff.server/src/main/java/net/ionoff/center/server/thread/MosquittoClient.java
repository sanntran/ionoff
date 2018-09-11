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

import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.SensorDriver;
import net.ionoff.center.server.exception.MqttConnectionException;
import net.ionoff.center.server.exception.MqttPublishException;
import net.ionoff.center.server.persistence.dao.IRelayDriverDao;
import net.ionoff.center.server.persistence.service.IDeviceService;
import org.springframework.beans.factory.annotation.Value;


public class MosquittoClient implements MqttCallback {
	
	private static Logger LOGGER = Logger.getLogger(MosquittoClient.class.getName());

	private boolean shutdown;
	private MqttClient client;
	private MqttConnectOptions connOpt;
	private boolean connected = false;
	private String[] subscribleTopics;

	private Thread mosquittoThread;

	@Value("${mqtt.user}")
	private String user;

	@Value("${mqtt.pass}")
	private String password;

	@Value("${mqtt.qos:2}")
	private Integer qos;

	private String clientId;

	@Value("${mqtt.broker.url}")
	private String brockerUrl;

	@Value("${mqtt.topic.ionoff.net}")
	private String defaultTopic;

	@Value("${mqtt.topic.ionoff.relaydriver}")
	private String topicRelayDriver;

	@Value("${mqtt.topic.ionoff.sensordriver}")
	private String topicSensorDriver;

	@Autowired
	private IDeviceService deviceService;
	
	@Autowired
	private IRelayDriverDao relayDriverDao;

	@Autowired
	private RelayDriverStatusHandler relayDriverStatusHandler;
	
	public void initAndConnectBroker() {
		clientId = "ionoff-" + hashCode() + "";
		subscribleTopics = new String[] {defaultTopic, topicRelayDriver, topicSensorDriver};
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
			LOGGER.info("Connected to broker" + brockerUrl);
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
		if (defaultTopic.equals(topic) || topicRelayDriver.equals(topic)) {
			onRelayDriverMessageArrived(payload);
		}
		else if (topicSensorDriver.equals(topic)) {
			onSensorDriverMessageArrived(payload);
		}
	}
	
	private void onSensorDriverMessageArrived(String payload) {		
		SensorDriverMqttPayload data = new SensorDriverMqttPayload(payload);
		SensorDriver sensorDriver = deviceService.findSensorDriverByMac(data.getId());
		if (sensorDriver == null) {
			LOGGER.info("Found no sensor-driver by id: " + data.getId());
			return;
		}
		Date now = new Date();
		sensorDriver.setTime(now);
		if (sensorDriver.getSensors() == null || sensorDriver.getSensors().isEmpty()) {
			deviceService.update(sensorDriver);
			return;
		}

		if (data.getValue() == null || data.getIndex() == null) {
			LOGGER.info("Invalid message format");
			deviceService.update(sensorDriver);
			return;
		}

		Sensor sensor = sensorDriver.getSensors().get(0);
		sensor.getStatus().setTime(now);
		sensor.getStatus().setValue(data.getValue());
		sensor.getStatus().setIndex(data.getIndex());
		
		deviceService.updateSensorStatus(sensor);
		if (SensorDriverMqttPayload.CHANGED.equals(data.getCode())) {
			deviceService.onSensorStatusChanged(sensor);
		}
	}

	private void onRelayDriverMessageArrived(String payload) {
		RelayDriverMqttPayload mqttPayload = new RelayDriverMqttPayload(payload);
		if (mqttPayload.getId() == null) {
			LOGGER.info("Message is not valid format " + payload);
			return;
		}
		List<RelayDriver> relayDrivers = relayDriverDao.findByMac(mqttPayload.getId());
		if (relayDrivers.isEmpty()) {
			LOGGER.info("No relay-driver found in the DB. Key: " + mqttPayload.getId());
			return;
		}
		
		RelayDriver relayDriver = relayDrivers.get(0);
		relayDriver.setConnectedTime(System.currentTimeMillis());
		relayDriverDao.update(relayDriver);
		if (!relayDriver.isConnected()) {
			LOGGER.info("RelayDriver " + relayDriver.getKey() + " is now connected");
		}
		if (RelayDriverMqttPayload.STATUS.equals(mqttPayload.getCode())) {
			//LOGGER.info("RelayDriver " + relayDriver.getKey() + " has been connected");
			relayDriverStatusHandler.onReceivedStatus(relayDriver, mqttPayload.getIn(),  mqttPayload.getOut());
		} else if (RelayDriverMqttPayload.CHANGED.equals(mqttPayload.getCode())) {
			//LOGGER.info("RelayDriver " + relayDriver.getKey() + " input status has been changed");
			relayDriverStatusHandler.onStatusChanged(relayDriver, mqttPayload.getIn(),  mqttPayload.getOut());
		} else if (RelayDriverMqttPayload.RESET.equals(mqttPayload.getCode())) {
			//LOGGER.info("RelayDriver " + relayDriver.getKey() + " has been started");
			relayDriverStatusHandler.onStarted(relayDriver, null, mqttPayload.getIn(),  mqttPayload.getOut());
		} else if (RelayDriverMqttPayload.CRASH.equals(mqttPayload.getCode())) {
			//LOGGER.info("RelayDriver " + relayDriver.getKey() + " started due to crash");
			relayDriverStatusHandler.onCrashed(relayDriver, mqttPayload.getIn(),  mqttPayload.getOut());
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
