package net.ionoff.center.server.driver.api;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.broker.MqttConnection;

public class E4RelayDriver implements IRelayDriver {
	
	@Autowired
	private MqttConnection mqttClient;
	
	@Override
	public void openRelay(RelayDriver relayDriver, int relayIndex) 
			throws RelayDriverException {
		checkConnected(relayDriver);
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
		checkConnected(relayDriver);
		int outId = relayIndex + 1;
		String req = "{ioseto" + outId + "0}";
		sendMqttMessage(relayDriver, req);
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

	@Override
	public void openRelay(RelayDriver driver, int relayIndex, Integer autoRevert) throws RelayDriverException {
		openRelay(driver, relayIndex);
	}

	@Override
	public void closeRelay(RelayDriver driver, int relayIndex, Integer autoRevert) throws RelayDriverException {
		if (autoRevert != null && autoRevert.intValue() == 1) {
			checkConnected(driver);
			int outId = relayIndex + 1;
			String req = "{ioseto" + outId + "2}";
			sendMqttMessage(driver, req);
			try {
				Thread.sleep(650);
			} catch (InterruptedException e) {
				//
			}
		}
		else {
			closeRelay(driver, relayIndex);
		}
	}

}
