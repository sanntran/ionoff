package net.ionoff.center.server.thread;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.List;
import java.util.Observable;
import java.util.Observer;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.persistence.dao.IRelayDriverDao;
import net.ionoff.center.server.relaydriver.api.ConnectionClosedEvent;
import net.ionoff.center.server.relaydriver.api.RecievedMessageEvent;
import net.ionoff.center.server.relaydriver.api.RelayDriverConnection;
import net.ionoff.center.server.relaydriver.api.RelayDriverConnectionMap;

public class RelayDriverConnectionPool extends Thread implements Observer {

	private static final Logger LOGGER = Logger.getLogger(RelayDriverConnectionPool.class.getName());

	private static final int TCP_SERVER_PORT = 8118;
	private static final String STARTED = "RS";
	private static final String CHANGED = "CH";
	private static final String STATUSS = "ST";

	private boolean shutdownFlag;
	private ServerSocket serverSocket;
	
	@Autowired
	private IRelayDriverDao relayDriverDao;

	@Autowired
	private RelayDriverStatusHandler relayDriverStatusHandler;

	public RelayDriverConnectionPool() {
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
						handleRelayDriverSocket(socket);
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

	private void handleRelayDriverSocket(Socket socket) throws Exception {
		RelayDriverConnection connectionHandler = new RelayDriverConnection(socket, this);
		connectionHandler.start();
	}

	private void addNewRelayDriverConnection(String mac, RelayDriverConnection connection) {
		RelayDriverConnection conn = RelayDriverConnectionMap.get(mac);
		if (conn != null) {
			conn.close();
			RelayDriverConnectionMap.removeConnection(mac);
		}
		//LOGGER.info("Add new relayDriver connection " + connection.toString() + " " + connection.getDate());
		RelayDriverConnectionMap.put(mac, connection);
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
		LOGGER.info("Stopping the relayDriver connection pool...");
		interrupt();
	}

	@Override
	public void update(Observable o, Object event) {
		if (event instanceof RecievedMessageEvent) { 
			RecievedMessageEvent messageEvent = (RecievedMessageEvent) event;
			RelayDriverConnection connection = messageEvent.getConnection();
			
			if (connection.getKey() == null) {
				LOGGER.info("Message recieved from " + messageEvent.getConnection().getIp() 
						+ "is not valid format: " + messageEvent.getConnection().getMessage());
			}
			else {
				List<RelayDriver> relayDrivers = relayDriverDao.findByMac(connection.getKey());
				if (!relayDrivers.isEmpty()) {
					addNewRelayDriverConnection(connection.getKey(), connection);
					RelayDriver relayDriver = relayDrivers.get(0);
					relayDriver.setConnectedTime(System.currentTimeMillis());
					relayDriverDao.update(relayDriver);
					if (!relayDriver.isConnected()) {
						LOGGER.info("RelayDriver " + messageEvent.getConnection().toString() + " is now connected");
					}
					if (connection.getMessage().startsWith(STATUSS)) {
						LOGGER.info("RelayDriver " + messageEvent.getConnection().toString() + " has been connected");
						relayDriverStatusHandler.onReceivedStatus(relayDriver, connection.getInput(), connection.getOutput());
					} else if (connection.getMessage().startsWith(CHANGED)) {
						LOGGER.info("RelayDriver " + messageEvent.getConnection().toString() + " input status has been changed");
						relayDriverStatusHandler.onStatusChanged(relayDriver, connection.getInput(), connection.getOutput());
					} else if (connection.getMessage().startsWith(STARTED)) {
						LOGGER.info("RelayDriver " + messageEvent.getConnection().toString() + " has been started");
						relayDriverStatusHandler.onStarted(relayDriver, connection.getIp(), connection.getInput(), connection.getOutput());
					}
				} else {
					LOGGER.info("No relayDriver found in the DB (" + messageEvent.getConnection().toString() + ")");
				}
			}
		}
		else if (event instanceof ConnectionClosedEvent) {
			//
		}
	}
}
