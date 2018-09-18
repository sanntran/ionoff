package net.ionoff.broker.tcp;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

public class SocketManager extends Thread {

	private static final Logger LOGGER = LoggerFactory.getLogger(SocketManager.class);

	// Map of client-id, socket-id
	private final Map<String, String> socketIds = new ConcurrentHashMap<>();

	// Map of socket-id, socket-handler
	private final Map<String, SocketHandler> socketHandlers = new ConcurrentHashMap<>();

	@Override
	public void run() {
		try {
			Thread.sleep(100);
			closeSocketIfTimeOut();
		} catch (InterruptedException e) {
			LOGGER.error("InterruptedException: " + e.getMessage());
		}
	}

	private void closeSocketIfTimeOut() {
		Iterator<Map.Entry<String, SocketHandler>> iterator = socketHandlers.entrySet().iterator();
		while (iterator.hasNext()) {
			Map.Entry<String, SocketHandler> entry = iterator.next();
			if (entry.getValue().isTimeOut()) {

			}
		}
	}

	public SocketHandler getSocketHandler(String clientId) throws NoSocketException {
		if (clientId == null) {
			throw new NoSocketException(clientId);
		}
		synchronized (clientId) {
			return getSocketHandler(clientId, 0);
		}
	}

	private SocketHandler getSocketHandler(String clientId, int retry) throws NoSocketException {

		String socketId = socketIds.get(clientId);
		if (socketId == null) {
			throw new NoSocketException(clientId);
		}
		if (retry >= 4) {
			throw new NoSocketException(clientId);
		}
		SocketHandler socketHandler =  socketHandlers.get(socketId);
		if (socketHandler == null) {
			throw new NoSocketException(clientId);
		}
		if (socketHandler.isLocked()) {
			if (socketHandler.getDuration() < SocketHandler.DEFAULT_TIMEOUT) {
				try {
					Thread.sleep(1000);
					return getSocketHandler(clientId, retry + 1);
				} catch (InterruptedException e) {
					// Ignore this exception
				}
			} else {
				throw new NoSocketException(clientId);
			}
		}
		return socketHandler;
	}

	public void removeSocketHandler(String clientId) {
		if (clientId == null) {
			return;
		}
		String socketId = socketIds.get(clientId);
		if (socketId == null) {
			return;
		}
		socketIds.remove(clientId);
		socketHandlers.remove(socketId);
	}

	public SocketHandler findSocketHandler(String clientId) {
		if (clientId == null) {
			return null;
		}
		return socketHandlers.get(socketIds.get(clientId));
	}

	public void putToSocketIds(String clientId, String socketId) {
		socketIds.put(clientId, socketId);
	}

	public void putToSocketHandlers(String socketId, SocketHandler socketHandler) {
		socketHandlers.put(socketId, socketHandler);
	}
}
