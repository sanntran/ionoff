package net.ionoff.center.server.relaydriver.api;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.thread.MosquittoClient;

public class E3RelayDriverApi implements IRelayDriverApi {
	
	@Autowired
	private MosquittoClient mqttClient;
	
	@Override
	public void openRelay(RelayDriver relayDriver, int relayIndex) 
			throws RelayDriverException {
		checkConnected(relayDriver);
		String req = getMqttMessage(relayIndex, 1, 0);
		sendMqttMessage(relayDriver, req);
	}
	
	private void sendMqttMessage(RelayDriver relayDriver, String req) {
		mqttClient.publishMessage(relayDriver.getKey(), req);
	}

	@Override
	public void closeRelay(RelayDriver relayDriver, int relayIndex) 
			throws RelayDriverException {
		checkConnected(relayDriver);
		Relay relay = relayDriver.getRelayByIdx(relayIndex);
		String req = getMqttMessage(relay.getIndex(), 0, 0);
		sendMqttMessage(relayDriver, req);
	}

	@Override
	public void openRelay(RelayDriver relayDriver, int relayIndex, Integer autoRevert) 
			throws RelayDriverException {
		checkConnected(relayDriver);
		if (autoRevert == null || autoRevert.intValue() < 0) {
			autoRevert = 0;
		}
		String req = getMqttMessage(relayIndex, 1, autoRevert);
		sendMqttMessage(relayDriver, req);
		if (autoRevert != null && autoRevert.intValue() == 1) {
			try {
				Thread.sleep(650);
			} catch (InterruptedException e) {
				//
			}
		}
	}
	
	@Override
	public void closeRelay(RelayDriver relayDriver, int relayIndex, Integer autoRevert) 
			throws RelayDriverException {
		checkConnected(relayDriver);
		if (autoRevert == null || autoRevert.intValue() < 0) {
			autoRevert = 0;
		}
		Relay relay = relayDriver.getRelayByIdx(relayIndex);
		String req = getMqttMessage(relay.getIndex(), 0, autoRevert);
		sendMqttMessage(relayDriver, req);
		if (autoRevert != null && autoRevert.intValue() == 1) {
			try {
				Thread.sleep(650);
			} catch (InterruptedException e) {
				//
			}
		}
	}
	
	private String getMqttMessage(int index, int state, int time) {
		StringBuilder builder = new StringBuilder();
		builder.append("{ioseto").append(index + 1).append(state).append(time).append("}");
		return builder.toString();
	}
	
	private void checkConnected(RelayDriver relayDriver) {
		if (!relayDriver.isConnected()) {
			throw new RelayDriverConnectException(relayDriver.getName());
		}
	}

	@Override
	public RelayDriverStatus getStatus(RelayDriver relayDriver) throws RelayDriverException {
		RelayDriverStatus status = new RelayDriverStatus();
		for (int i = 0; i < relayDriver.getRelays().size(); i++) {
			// Default input status of E4 is always true
			// Input status of E4 should be store in DB instead of hard coding here
			status.getDigitalInputStatus().add(true);
			status.getRelayOutputStatus().add(relayDriver.getRelays().get(i).getStatus());
		}
		return status;
	}
}
