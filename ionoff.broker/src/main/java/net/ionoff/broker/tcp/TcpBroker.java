package net.ionoff.broker.tcp;

import net.ionoff.broker.mqtt.MqttBroker;
import net.ionoff.broker.tcp.handler.SocketHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

public class TcpBroker extends Thread {

	private static final Logger LOGGER = LoggerFactory.getLogger(TcpBroker.class);

	private static final SocketManager SOCKET_MANAGER = new SocketManager();
	private static final int TCP_SERVER_PORT = 8118;

	private ServerSocket serverSocket;
	private final MqttBroker mqttBroker;

	public TcpBroker(MqttBroker mqttBroker) {
		this.mqttBroker = mqttBroker;
	}

	@Override
	public void run() {
		LOGGER.info("TCP server is listening on port " + TCP_SERVER_PORT);
		try {
			SOCKET_MANAGER.start();
			serverSocket = new ServerSocket(TCP_SERVER_PORT);
			for (; true;) {
				try {
					if (serverSocket.isClosed()) {
						break;
					}
					final Socket socket = serverSocket.accept();
					try {
						handleSocket(socket);
					} catch (final Throwable t) {
						socket.close();
						LOGGER.error(t.getMessage(), t);
					}
				} catch (final Throwable t) {
					LOGGER.error(t.getMessage(), t);
				}
			}
		} catch (final IOException e) {
			LOGGER.error(e.getMessage(), e);
		}
	}

	private void handleSocket(Socket socket) throws Exception {
		SocketHandler requestHandler = new SocketHandler(socket, this, mqttBroker);
		SOCKET_MANAGER.putToSocketHandlers(requestHandler.getSocketId(), requestHandler);
		requestHandler.start();
	}

	public String sendCommand(String clientId, String command) throws IOException {
		SocketHandler socketHandler = SOCKET_MANAGER.getSocketHandler(clientId);
		if (socketHandler == null || socketHandler.isLocked()) {
			throw new NoSocketException(clientId);
		}
		return socketHandler.sendCommand(command);
	}

	public SocketManager getSocketManager() {
		return SOCKET_MANAGER;
	}
}
