package net.ionoff.center.server.controller.api;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.thread.MosquittoClient;

public class E4ControllerApi implements IControllerApi {
	
	@Autowired
	private MosquittoClient mqttClient;
	
	@Override
	public void openRelay(Controller controller, int relayIndex) 
			throws ControllerException {
		verifyController(controller);
		int outId = relayIndex + 1;
		String req = "{ioseto" + outId + "1}";
		sendMqttMessage(controller, req);
	}
	
	private void sendMqttMessage(Controller controller, String req) {
		mqttClient.publishMessage(controller.getKey(), req);
	}

	@Override
	public void closeRelay(Controller controller, int relayIndex) 
			throws ControllerException {
		verifyController(controller);
		int outId = relayIndex + 1;
		String req = "{ioseto" + outId + "0}";
		sendMqttMessage(controller, req);
	}
	
	@Override
	public void closeOpenRelay(Controller controller, int relayIndex) 
			throws ControllerException {		
		closeRelay(controller, relayIndex);
		try {
			Thread.sleep(1000);
		} catch (InterruptedException e) {
			//
		}
		openRelay(controller, relayIndex);
	}
	
	private void verifyController(Controller controller) {
		if (!controller.isConnected()) {
			throw new ControllerConnectException(controller.getName());
		}
		if (!controller.isValidKey()) {
			throw new ControllerApiException("Controller key invalid: " + controller.getKey());
		}
	}

	@Override
	public ControllerStatus getStatus(Controller controller) throws ControllerException {
		ControllerStatus status = new ControllerStatus();
		for (int i = 0; i < controller.getRelays().size(); i++) {
			// Default input status of E4 is always true
			// Input status of E4 should be store in DB instead of hard coding here
			status.getDigitalInputStatus().add(true);
			status.getRelayOutputStatus().add(controller.getRelays().get(i).getStatus());
		}
		return status;
	}
}
