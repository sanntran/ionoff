package net.ionoff.center.server.xapxinh;

import java.net.InetAddress;
import java.util.Date;
import java.util.Map;

import org.apache.log4j.Logger;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.persistence.dao.IDeviceDao;
import net.xapxinh.center.server.api.player.PlayerConnection;
import net.xapxinh.center.server.entity.Player;
import net.xapxinh.center.server.service.player.AbstractPlayerConnectionPool;
import org.springframework.beans.factory.annotation.Value;

public class PlayerConnectionPool extends AbstractPlayerConnectionPool {

	private static final Logger LOGGER = Logger.getLogger(PlayerConnectionPool.class.getName());

	@Value("${tcpserver.player.port}")
	private Integer  tcpServerPlayerPort;

	@Autowired
	private IDeviceDao deviceDao;

	@Override
	protected Player getPlayer(String mac){
		net.ionoff.center.server.entity.Player iPlayer = deviceDao.findPlayerByMac(mac);
		if (iPlayer == null) {
			LOGGER.error("There is no player has MAC: " + mac);
			return null;
		}
		Player player = new Player();
		player.setId(iPlayer.getId());
		player.setMac(mac);
		player.setName(iPlayer.getName());
		return player;
	}

	@Override
	protected int getTcpPort() {
		return tcpServerPlayerPort;
	}
	
	@Override
	protected void handleIntervalPingingMessage(Map<String, String> soketMessage) {
		try {
			JSONObject obj = new JSONObject(soketMessage);
			if (obj.has("mac")) {
				String mac = (String) obj.get("mac");
				net.ionoff.center.server.entity.Player iPlayer 
					= deviceDao.findPlayerByMac(mac);
				if (iPlayer != null) {
					iPlayer.setTime(new Date());
					deviceDao.update(iPlayer);
				}
				else {
					LOGGER.error("There is no player has MAC: " + mac);
				}
			}
		}
		catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		}
		
	}

	@Override
	protected void onAcceptedConnection(PlayerConnection connection) {
		net.ionoff.center.server.entity.Player iPlayer = 
				(net.ionoff.center.server.entity.Player) deviceDao.findById(connection.getId());
		
		if (iPlayer != null) {
			InetAddress address = connection.getSocket().getInetAddress();
			if (address != null) {
				iPlayer.setIp(address.getHostAddress());
			}
			// make the player be connected
			iPlayer.setTime(new Date());
			deviceDao.update(iPlayer);
		}
	}

	@Override
	protected void onRemovedConnection(Long connectionId) {
		net.ionoff.center.server.entity.Player iPlayer = 
				(net.ionoff.center.server.entity.Player) deviceDao.findById(connectionId);
		if (iPlayer != null) {
			// make the player be not connected
			iPlayer.setTime(new Date(System.currentTimeMillis() - 60000));
			deviceDao.update(iPlayer);
		}
	}
}
