package net.ionoff.center.server.thread;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.xapxinh.PlayerConnectionPool;

public class ServerThreadPool {
	
	private static final Logger LOGGER = Logger.getLogger(ServerThreadPool.class.getName());

	private boolean started;
	
	private boolean shutdown;
	
	@Autowired
	private MosquittoClient mosquittoClient;
	
	@Autowired
	private PlayerConnectionPool playerConnectionPool;
	
	@Autowired
	private RelayDriverConnectionPool relayDriverConnectionPool;
	
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
		playerConnectionPool.start();
		relayDriverConnectionPool.start();
	}

	public void shutdown() {
		this.shutdown = true;
		
		LOGGER.info("MosquittoClient is shutting down...");
		mosquittoClient.shutdown();
		// iupdator is updating...
		LOGGER.info("PlayerConnectionPool is shutting down...");
		playerConnectionPool.shutdown();
		// iupdator is updating...
		LOGGER.info("RelayDriverConnectionPool is shutting down...");
		relayDriverConnectionPool.shutdown();
	}

	public boolean isShutdown() {
		return shutdown;
	}
}
