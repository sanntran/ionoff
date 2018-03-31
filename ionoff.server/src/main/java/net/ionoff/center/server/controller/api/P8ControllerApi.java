package net.ionoff.center.server.controller.api;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.thread.ControllerConnectionPool;
import net.ionoff.center.shared.entity.ControllerModel;

public class P8ControllerApi implements IControllerApi {
	
	@Autowired 
	private ControllerConnectionPool controllerConnectionPool;
	
	@Override
	public ControllerStatus getStatus(Controller controller) 
			throws ControllerException {
		if (!controller.isConnected()) {
			throw new ControllerConnectException(controller.getName());
		}
		if (!controller.isValidKey()) {
			throw new ControllerApiException("Password invalid: " + controller.getKey());
		}
		String req = "{ioget}";
		String status = sendTcpCommand(controller, req);
		
		return toControllerStatus(controller.getModelObj(), status);
	}
	
	@Override
	public void openRelay(Controller controller, int relayIndex) 
			throws ControllerException {
		if (!controller.isConnected()) {
			throw new ControllerConnectException(controller.getName());
		}
		if (!controller.isValidKey()) {
			throw new ControllerApiException("Controller key invalid: " + controller.getKey());
		}
		int outId = relayIndex + 1;
		String req = "{ioseto" + outId + "0}";
		sendTcpCommand(controller, req);
	}
	
	@Override
	public void closeRelay(Controller controller, int relayIndex) 
			throws ControllerException {
		if (!controller.isConnected()) {
			throw new ControllerConnectException(controller.getName());
		}
		if (!controller.isValidKey()) {
			throw new ControllerApiException("Controller key invalid: " + controller.getKey());
		}
		int outId = relayIndex + 1;
		String req = "{ioseto" + outId + "1}";
		sendTcpCommand(controller, req);
	}
	
	@Override
	public void closeOpenRelay(Controller controller, int relayIndex)
			throws ControllerException {
		if (!controller.isConnected()) {
			throw new ControllerConnectException(controller.getName());
		}
		if (!controller.isValidKey()) {
			throw new ControllerApiException("Controller key invalid: " + controller.getKey());
		}
		int outId = relayIndex + 1;
		String req = "{ioseto" + outId + "2}";
		sendTcpCommand(controller, req);
	}
	
	protected String sendTcpCommand(Controller controller, String command) throws ControllerException {
		ControllerConnection connection = getConnection(controller);
		if (connection == null || connection.isClosed()) {
			//System.out.println("Connection is closed " + connection.getDate());
			throw new ControllerConnectException(controller.getName());
		}
		return sendTcpRequestStatus(connection, command);
	}

	protected ControllerStatus toControllerStatus(ControllerModel model, String message) throws ControllerApiException {
		String messages[] = message.split(":");
		if (messages.length != 2) {
			throw new ControllerApiException(message);
		}
		String statuses[] = messages[1].split(",");
		if (statuses.length != 2) {
			throw new ControllerApiException(message);
		}
		return ControllerStatus.fromIOStatusesString(model, statuses[0], statuses[1]);
	}
	
	private static synchronized String sendTcpRequestStatus(ControllerConnection connection, String req) 
			throws ControllerException {
		String response = "";
		try {
			response = connection.sendCommand(req);
		} catch (Exception e) {
			throw new ControllerConnectException(connection.getKey());
		}
		if (response == null || response.isEmpty()) {
			throw new ControllerConnectException(connection.getKey());
		}
		if (response.startsWith("404")) {
			throw new ControllerApiException("API Error: " + connection.getKey());
		}
		return response;
	}
	
	private ControllerConnection getConnection(Controller controller) throws ControllerConnectException {
		return controllerConnectionPool.getControllerConnection(controller); 
	}
}
