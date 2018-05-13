package net.ionoff.center.server.relaydriver.api;

import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.shared.entity.RelayDriverModel;

public class P8RelayDriverApi implements IRelayDriverApi {
	
	@Override
	public RelayDriverStatus getStatus(RelayDriver relayDriver) 
			throws RelayDriverException {
		if (!relayDriver.isConnected()) {
			throw new RelayDriverConnectException(relayDriver.getName());
		}
		if (!relayDriver.isValidKey()) {
			throw new RelayDriverApiException("Password invalid: " + relayDriver.getKey());
		}
		String req = "{ioget}";
		String status = sendTcpCommand(relayDriver, req);
		
		return toRelayDriverStatus(relayDriver.getModelObj(), status);
	}
	
	@Override
	public void openRelay(RelayDriver relayDriver, int relayIndex) 
			throws RelayDriverException {
		if (!relayDriver.isConnected()) {
			throw new RelayDriverConnectException(relayDriver.getName());
		}
		if (!relayDriver.isValidKey()) {
			throw new RelayDriverApiException("RelayDriver key invalid: " + relayDriver.getKey());
		}
		int outId = relayIndex + 1;
		String req = "{ioseto" + outId + "0}";
		sendTcpCommand(relayDriver, req);
	}
	
	@Override
	public void closeRelay(RelayDriver relayDriver, int relayIndex) 
			throws RelayDriverException {
		if (!relayDriver.isConnected()) {
			throw new RelayDriverConnectException(relayDriver.getName());
		}
		if (!relayDriver.isValidKey()) {
			throw new RelayDriverApiException("RelayDriver key invalid: " + relayDriver.getKey());
		}
		int outId = relayIndex + 1;
		String req = "{ioseto" + outId + "1}";
		sendTcpCommand(relayDriver, req);
	}
	
	@Override
	public void closeOpenRelay(RelayDriver relayDriver, int relayIndex)
			throws RelayDriverException {
		if (!relayDriver.isConnected()) {
			throw new RelayDriverConnectException(relayDriver.getName());
		}
		if (!relayDriver.isValidKey()) {
			throw new RelayDriverApiException("RelayDriver key invalid: " + relayDriver.getKey());
		}
		int outId = relayIndex + 1;
		String req = "{ioseto" + outId + "2}";
		sendTcpCommand(relayDriver, req);
	}
	
	protected String sendTcpCommand(RelayDriver relayDriver, String command) throws RelayDriverException {
		RelayDriverConnection connection = getConnection(relayDriver);
		if (connection == null || connection.isClosed()) {
			//System.out.println("Connection is closed " + connection.getDate());
			throw new RelayDriverConnectException(relayDriver.getName());
		}
		return sendTcpRequestStatus(connection, command);
	}

	protected RelayDriverStatus toRelayDriverStatus(RelayDriverModel model, String message) throws RelayDriverApiException {
		String messages[] = message.split(":");
		if (messages.length != 2) {
			throw new RelayDriverApiException(message);
		}
		String statuses[] = messages[1].split(",");
		if (statuses.length != 2) {
			throw new RelayDriverApiException(message);
		}
		return RelayDriverStatus.fromIOStatusesString(model, statuses[0], statuses[1]);
	}
	
	private static synchronized String sendTcpRequestStatus(RelayDriverConnection connection, String req) 
			throws RelayDriverException {
		String response = "";
		try {
			response = connection.sendCommand(req);
		} catch (Exception e) {
			throw new RelayDriverConnectException(connection.getKey());
		}
		if (response == null || response.isEmpty()) {
			throw new RelayDriverConnectException(connection.getKey());
		}
		if (response.startsWith("404")) {
			throw new RelayDriverApiException("API Error: " + connection.getKey());
		}
		return response;
	}
	private RelayDriverConnection getConnection(RelayDriver relayDriver) throws RelayDriverConnectException {
		return RelayDriverConnectionMap.getRelayDriverConnection(relayDriver); 
	}
}
