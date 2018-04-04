package net.ionoff.things.mqtt;

import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;
import org.eclipse.paho.client.mqttv3.MqttCallback;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;

public abstract class MosquittoClient implements MqttCallback {
	
	private MqttClient client;
	private MqttConnectOptions connOpt;
	private boolean connected = false;
	private String publishTopic;
	private String subscribleTopic;
	private long messageArrivedTime = 0;
	
	public MosquittoClient() {
		try {
			client = new MqttClient(UserConfig.MQTT_URL, "IConfigTool");
		} catch (MqttException e) {
			e.printStackTrace();
		}
		client.setCallback(this);
		connOpt = new MqttConnectOptions();
		connOpt.setCleanSession(true);
		//connOpt.setKeepAliveInterval(60);
		connOpt.setUserName(UserConfig.MQTT_USER);
		connOpt.setPassword(UserConfig.MQTT_PASSWORD.toCharArray());
		connOpt.setCleanSession(true);
	}

	public void publishMessage(String payload) throws Exception {
		MqttMessage message = new MqttMessage(payload.getBytes());
        message.setQos(2);
        System.out.println("Publishing message:" + message);
        client.publish(publishTopic, message);
	}
	
	@Override
	public void connectionLost(Throwable cause) {
		cause.printStackTrace();
		setConnected(false);
		onConnectionLost();
		// Called when the connection to the server has been lost.
		// An application may choose to implement reconnection logic at this
		// point.
		System.out.println("Connection to " + UserConfig.MQTT_URL + " lost! " + cause);
		try {
			Thread.sleep(5000);
		} catch (InterruptedException e) {
			System.out.println(e.getMessage());
		}
		connectBroker();
	}

	public void connectBroker() {
		try {
			System.out.println("Connecting to broker...");
			client.connect(connOpt);
			System.out.println("Connected to broker");
			setConnected(true);
			onConnectedBroker();
		} catch (MqttException e) {
			e.printStackTrace();
			try {
				Thread.sleep(5000);
			} catch (InterruptedException e1) {
				e1.printStackTrace();
			}
			connectBroker();
		}
	}
	
	private void setConnected(boolean b) {
		// TODO Auto-generated method stub
		
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
		System.out.println("Delivery message to broker complete. " + token.getMessageId());
	}
	
	@Override
	public void messageArrived(String topic, MqttMessage message) throws Exception {
		messageArrivedTime = System.currentTimeMillis();
		// Called when a message arrives from the server that matches any
		// subscription made by the client
		String payload = new String(message.getPayload());
		System.out.println("MessageArrived: " + payload);
		onMessageArrived(topic, payload);
	}
	
	public abstract void onConnectedBroker();
	
	public abstract void onConnectionLost();
	
	public abstract void onMessageArrived(String topic, String message);

	public void setPublishTopic(String publishTopic) {
		this.publishTopic = publishTopic;
	}

	public void setSubscribleTopic(String subscribleTopic) {
		if (this.subscribleTopic != null) {
			try {
				client.unsubscribe(this.subscribleTopic);
			} catch (MqttException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		this.subscribleTopic = subscribleTopic;
		try {
			client.subscribe(this.subscribleTopic);
		} catch (MqttException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public boolean isThingOnline() {
		return System.currentTimeMillis() - messageArrivedTime < 60000;
	}

	public boolean isConnected() {
		return connected;
	}
}
