package net.ionoff.center.server.relaydriver.api;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.thread.MosquittoClient;

public class E4RelayDriverApi implements IRelayDriverApi {
	
	@Autowired
	private MosquittoClient mqttClient;
	
	@Override
	public void openRelay(RelayDriver relayDriver, int relayIndex) 
			throws RelayDriverException {
		verifyRelayDriver(relayDriver);
		int outId = relayIndex + 1;
		String req = "{ioseto" + outId + "1}";
		sendMqttMessage(relayDriver, req);
	}
	
	private void sendMqttMessage(RelayDriver relayDriver, String req) {
		mqttClient.publishMessage(relayDriver.getKey(), req);
	}

	@Override
	public void closeRelay(RelayDriver relayDriver, int relayIndex) 
			throws RelayDriverException {
		verifyRelayDriver(relayDriver);
		int outId = relayIndex + 1;
		String req = "{ioseto" + outId + "0}";
		sendMqttMessage(relayDriver, req);
	}
	
	@Override
	public void closeOpenRelay(RelayDriver relayDriver, int relayIndex) 
			throws RelayDriverException {		
		closeRelay(relayDriver, relayIndex);
		try {
			Thread.sleep(1000);
		} catch (InterruptedException e) {
			//
		}
		openRelay(relayDriver, relayIndex);
	}
	
	private void verifyRelayDriver(RelayDriver relayDriver) {
		if (!relayDriver.isConnected()) {
			throw new RelayDriverConnectException(relayDriver.getName());
		}
		if (!relayDriver.isValidKey()) {
			throw new RelayDriverApiException("RelayDriver key invalid: " + relayDriver.getKey());
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
