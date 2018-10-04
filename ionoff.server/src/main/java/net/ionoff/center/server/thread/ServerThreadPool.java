package net.ionoff.center.server.thread;

import net.ionoff.center.server.broker.MqttConnection;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

public class ServerThreadPool {
	
	private static final Logger LOGGER = Logger.getLogger(ServerThreadPool.class.getName());

	private boolean started;
	
	private boolean shutdown;
	
	@Autowired
	private MqttConnection mosquittoClient;

	
	public ServerThreadPool() {
		started = false;
		shutdown = false;
	}
	
	public void start() {
		if (started) {
			return;
		}
		started = true;
		mosquittoClient.start();
	}

	public void shutdown() {
		this.shutdown = true;
		
		LOGGER.info("MosquittoClient is shutting down...");
		mosquittoClient.shutdown();
		// iupdator is updating...
		LOGGER.info("RelayDriverConnectionPool is shutting down...");
	}

	public boolean isShutdown() {
		return shutdown;
	}
}
