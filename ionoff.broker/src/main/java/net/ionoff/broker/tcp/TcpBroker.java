package net.ionoff.broker.tcp;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

public class TcpBroker extends Thread {

	private static final Logger LOGGER = LoggerFactory.getLogger(TcpBroker.class);

	private static final SocketManager SOCKET_MANAGER = new SocketManager();

	private static final int TCP_SERVER_PORT = 8118;
	private static final String STARTED = "RS";
	private static final String CHANGED = "CH";
	private static final String STATUSS = "ST";

	private boolean shutdownFlag;
	private ServerSocket serverSocket;


	public TcpBroker() {
		shutdownFlag = false;
	}

	@Override
	public void run() {
		LOGGER.info("Thread has been started !");
		try {
			serverSocket = new ServerSocket(TCP_SERVER_PORT);
			for (; true;) {
				if (shutdownFlag) {
					for (; !serverSocket.isClosed();) {
						try {
							LOGGER.info("Closing server socket...");
							serverSocket.close();
						} catch (IOException e) {
							LOGGER.error(e.getMessage(), e);
						}
					}
					return;
				}
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
		SocketHandler requestHandler = new SocketHandler(socket, SOCKET_MANAGER);
		requestHandler.start();
	}


	public void shutdown() {
		shutdownFlag = true;
		for (; !serverSocket.isClosed();) {
			try {
				LOGGER.info("Closing relayDriver server socket...");
				serverSocket.close();
			} catch (Exception e) {
				LOGGER.error(e.getMessage(), e);
			}
		}
		LOGGER.info("Stopping the connection pool...");
		interrupt();
	}

	public String sendCommand(String clientId, String command) throws IOException {
		SocketHandler connection = SOCKET_MANAGER.getSocketHandler(clientId);
		if (connection == null || connection.isLocked()) {
			throw new NoSocketException(clientId);
		}
		return connection.sendCommand(command);
	}

}
