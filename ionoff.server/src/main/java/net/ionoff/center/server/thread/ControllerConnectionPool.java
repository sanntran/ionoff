package net.ionoff.center.server.thread;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.List;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.controller.api.ConnectionClosedEvent;
import net.ionoff.center.server.controller.api.ControllerConnectException;
import net.ionoff.center.server.controller.api.ControllerConnection;
import net.ionoff.center.server.controller.api.RecievedMessageEvent;
import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.persistence.service.IControllerService;

public class ControllerConnectionPool extends Thread implements Observer {

	private static final Logger LOGGER = Logger.getLogger(ControllerConnectionPool.class.getName());

	private Map<String, ControllerConnection> controllerConnections;

	private static final int TCP_SERVER_PORT = 8118;
	private static final String STARTED = "RS";
	private static final String CHANGED = "CH";
	private static final String STATUSS = "ST";

	@Autowired
	private IControllerService controllerService;
	
	@Autowired
	private ControllerStatusHandler controllerStatusHandler;

	private ServerSocket serverSocket;

	private boolean shutdownFlag;

	public ControllerConnectionPool() {
		shutdownFlag = false;
		controllerConnections = new ConcurrentHashMap<>();
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
						handleControllerSocket(socket);
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

	private void handleControllerSocket(Socket socket) throws Exception {
		ControllerConnection connectionHandler = new ControllerConnection(socket, this);
		connectionHandler.start();
	}

	private void addNewControllerConnection(String mac, ControllerConnection connection) {
		ControllerConnection conn = controllerConnections.get(mac);
		if (conn != null) {
			conn.close();
			controllerConnections.remove(mac);
		}
		//LOGGER.info("Add new controller connection " + connection.toString() + " " + connection.getDate());
		controllerConnections.put(mac, connection);
	}

	public ControllerConnection getControllerConnection(Controller controller) throws ControllerConnectException {
		synchronized (controller.getKey()) {
			return getControllerConnection(controller, 0);
		}
	}

	private ControllerConnection getControllerConnection(Controller controller, int retry)
			throws ControllerConnectException {
		if (retry >= 4) {
			throw new ControllerConnectException(controller.getName());
		}
		ControllerConnection conn = controllerConnections.get(controller.getKey());
		if (conn == null) {
			throw new ControllerConnectException(controller.getName());
		}
		if (conn.isClosed()) {
			if (controller.isConnected()) {
				try {
					Thread.sleep(1000);
					return getControllerConnection(controller, retry + 1);
				} catch (InterruptedException e) {
					// Ignore this exception
				}
			} else {
				throw new ControllerConnectException(controller.getName());
			}
		}
		return conn;
	}

	

	public void shutdown() {
		shutdownFlag = true;
		for (; !serverSocket.isClosed();) {
			try {
				LOGGER.info("Closing controller server socket...");
				serverSocket.close();
			} catch (Exception e) {
				LOGGER.error(e.getMessage(), e);
			}
		}
		LOGGER.info("Stopping the controller connection pool...");
		interrupt();
	}

	@Override
	public void update(Observable o, Object event) {
		if (event instanceof RecievedMessageEvent) {
			RecievedMessageEvent messageEvent = (RecievedMessageEvent) event;
			ControllerConnection connection = messageEvent.getConnection();
			
			if (connection.getKey() == null) {
				LOGGER.info("Message recieved from " + messageEvent.getConnection().getIp() 
						+ "is not valid format: " + messageEvent.getConnection().getMessage());
			}
			else {
				List<Controller> controllers = controllerService.findByMac(connection.getKey());
				if (!controllers.isEmpty()) {
					addNewControllerConnection(connection.getKey(), connection);
					Controller controller = controllers.get(0);
					controller.setConnectedTime(System.currentTimeMillis());
					controllerService.update(controller);
					if (!controller.isConnected()) {
						LOGGER.info("Controller " + messageEvent.getConnection().toString() + " is now connected");
					}
					if (connection.getMessage().startsWith(STATUSS)) {
						LOGGER.info("Controller " + messageEvent.getConnection().toString() + " has been connected");
						controllerStatusHandler.onReceivedControllerStatus(controller, connection.getInput(), connection.getOutput());
					} else if (connection.getMessage().startsWith(CHANGED)) {
						LOGGER.info("Controller " + messageEvent.getConnection().toString() + " input status has been changed");
						controllerStatusHandler.onControllerStatusChanged(controller, connection.getInput(), connection.getOutput());
					} else if (connection.getMessage().startsWith(STARTED)) {
						LOGGER.info("Controller " + messageEvent.getConnection().toString() + " has been started");
						controllerStatusHandler.onControllerStarted(controller, connection.getIp(), connection.getInput(), connection.getOutput());
					}
				} else {
					LOGGER.info("No controller found in the DB (" + messageEvent.getConnection().toString() + ")");
				}
			}
		}
		else if (event instanceof ConnectionClosedEvent) {
			//
		}
	}
}
