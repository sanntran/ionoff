package net.ionoff.center.server.driver.api;

import java.util.HashMap;
import java.util.Map;

import net.ionoff.center.server.entity.RelayDriver;

public class RelayDriverConnectionMap {
	
	private static final Map<String, RelayDriverConnection> RELAY_DRIVER_CONNECTIONS = new HashMap<>();

	public static RelayDriverConnection getRelayDriverConnection(RelayDriver relayDriver)
			throws RelayDriverConnectException {
		synchronized (relayDriver.getKey()) {
			return getRelayDriverConnection(relayDriver, 0);
		}
	}

	private static RelayDriverConnection getRelayDriverConnection(RelayDriver relayDriver, int retry)
			throws RelayDriverConnectException {
		if (retry >= 4) {
			throw new RelayDriverConnectException(relayDriver.getName());
		}
		RelayDriverConnection conn = RelayDriverConnectionMap.RELAY_DRIVER_CONNECTIONS.get(relayDriver.getKey());
		if (conn == null) {
			throw new RelayDriverConnectException(relayDriver.getName());
		}
		if (conn.isClosed()) {
			if (relayDriver.isConnected()) {
				try {
					Thread.sleep(1000);
					return getRelayDriverConnection(relayDriver, retry + 1);
				} catch (InterruptedException e) {
					// Ignore this exception
				}
			} else {
				throw new RelayDriverConnectException(relayDriver.getName());
			}
		}
		return conn;
	}

	public static void removeConnection(String mac) {
		RELAY_DRIVER_CONNECTIONS.remove(mac);
	}

	public static RelayDriverConnection get(String mac) {
		return RELAY_DRIVER_CONNECTIONS.get(mac);
	}

	public static void put(String mac, RelayDriverConnection connection) {
		RELAY_DRIVER_CONNECTIONS.put(mac, connection);
	}
}
