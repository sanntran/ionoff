package net.xapxinh.center.server.service.player;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;

import org.apache.log4j.Logger;
import org.json.JSONException;
import org.json.JSONObject;

import net.xapxinh.center.server.api.player.PlayerConnection;
import net.xapxinh.center.server.api.player.RecievedMessageEvent;
import net.xapxinh.center.server.entity.Player;

public abstract class AbstractPlayerConnectionPool extends Thread implements Observer {

	private static final Logger LOGGER = Logger.getLogger(AbstractPlayerConnectionPool.class.getName());

	private ServerSocket serverSocket;
	private final Map<Long, PlayerConnection> playerConnections;

	public AbstractPlayerConnectionPool() {
		playerConnections = new HashMap<Long, PlayerConnection>();
	}

	protected abstract Player getPlayer(String mac);

	protected abstract int getTcpPort();

	@Override
	public void run() {
		LOGGER.info("Start " + AbstractPlayerConnectionPool.class.getName());
		try {
			serverSocket = new ServerSocket(getTcpPort());
			for (; true;) {
				try {
					if (serverSocket.isClosed()) {
						break;
					}
					final Socket socket = serverSocket.accept();
					handleNewReceivedSoket(socket);
				} catch (final Throwable t) {
					LOGGER.error(t.getMessage(), t);
				}
			}
		} catch (final IOException e) {
			LOGGER.error(e.getMessage(), e);
		}
	}

	public void shutdown() {
		for (; !serverSocket.isClosed();) {
			try {
				LOGGER.info("Closing server socket...");
				serverSocket.close();
			} catch (Exception e) {
				LOGGER.error(e.getMessage(), e);
			}
		}
		LOGGER.info("Stopping the connection pool...");
		interrupt();
	}

	private void handleNewReceivedSoket(Socket socket) throws Exception {
		PlayerConnection connection = new PlayerConnection(socket, this);
		connection.start();
	}

	private void sendResponseStatus(PlayerConnection playerConnection) throws JSONException {
		final JSONObject response = new JSONObject();
		response.put("status", "connected");
		response.put("playerId", playerConnection.getId());
		response.put("playerName", playerConnection.getName());
		playerConnection.getWriter().println(response.toString());
	}

	private void acceptConnection(PlayerConnection connection) {
		// May be this new connection is from a existed Player
		if (playerConnections.containsKey(connection.getId())) {
			// In this case, the connection from existed Player should be removed
			// because it will not be used any more and replaced be the new one
			playerConnections.get(connection.getId()).close();
			LOGGER.info("Remove connection " + connection.getId() + " from connection pool");
			playerConnections.remove(playerConnections.get(connection.getId()));
		}
		LOGGER.info("Put connection " + connection.getId() + " to connection pool");
		playerConnections.put(connection.getId(), connection);
		onAcceptedConnection(connection);
	}

	/**
	 * @param connectionId
	 *            Player ID
	 */
	protected abstract void onAcceptedConnection(PlayerConnection connectionId);

	public PlayerConnection getPlayerConnection(Long playerId) {
		if (playerConnections.containsKey(playerId)) {
			return playerConnections.get(playerId);
		}
		return null;
	}

	public boolean hasPlayerConnection(Long playerId) {
		if (playerConnections.containsKey(playerId)) {
			return true;
		}
		return false;
	}

	public void removePlayerConnection(long playerId) {
		if (playerConnections.containsKey(playerId)) {
			playerConnections.get(playerId).close();
			LOGGER.info("Remove connection " + playerId + " from connection pool");
			playerConnections.remove(playerId);
			onRemovedConnection(playerId);
		}
	}

	/**
	 * @param connectionId
	 *            Player ID
	 */
	protected abstract void onRemovedConnection(Long connectionId);

	protected void handleIntervalPingingMessage(Map<String, String> soketMessage) {
		// does nothing by default
	}

	@Override
	public void update(Observable o, Object event) {
		if (event instanceof RecievedMessageEvent) {
			RecievedMessageEvent messageEvent = (RecievedMessageEvent) event;
			if (messageEvent.getConnection().isKeepAlive()) {
				String mac = messageEvent.getMessages().get("mac");
				final Player player = getPlayer(mac);
				try {
					if (player == null) {
						String errMessage = "Player connection is not accepted: " + mac;
						LOGGER.error(errMessage);
						final JSONObject response = new JSONObject();
						response.put("status", "failed");
						response.put("message", errMessage);
						messageEvent.getConnection().getWriter().println(response.toString());
					} else {
						messageEvent.getConnection().setId(player.getId());
						messageEvent.getConnection().setName(player.getName());
						messageEvent.getConnection().setPass(messageEvent.getMessages().get("pass"));
						messageEvent.getConnection().setPassword(messageEvent.getMessages().get("password"));

						acceptConnection(messageEvent.getConnection());
						sendResponseStatus(messageEvent.getConnection());
					}
				} catch (Exception e) {
					LOGGER.error(e.getMessage());
				}

			} else {
				handleIntervalPingingMessage(messageEvent.getMessages());
				final JSONObject response = new JSONObject();
				response.put("status", "ok");
				messageEvent.getConnection().getWriter().println(response.toString());
				messageEvent.getConnection().close();
			}
		}
	}
}
