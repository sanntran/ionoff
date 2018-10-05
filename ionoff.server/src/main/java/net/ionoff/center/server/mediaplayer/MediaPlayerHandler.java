package net.ionoff.center.server.mediaplayer;

import com.google.gson.Gson;
import net.ionoff.center.server.entity.Player;
import net.ionoff.center.server.mediaplayer.cache.PlayerCaches;
import net.ionoff.center.server.mediaplayer.model.MediaPlayerMessage;
import net.ionoff.center.server.persistence.service.IDeviceService;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Date;

public class MediaPlayerHandler {

	private static final Logger LOGGER = Logger.getLogger(MediaPlayerHandler.class.getName());
	private static final Gson GSON = new Gson();
	@Autowired
	private IDeviceService deviceService;

	@Autowired
	private PlayerCaches playerCaches;

	public void onMessageArrived(String payload) {
		LOGGER.info("Received media player message " + payload);
		try {
			MediaPlayerMessage message = GSON.fromJson(payload, MediaPlayerMessage.class);
			Player player = deviceService.findPlayerByMac(message.getMac());
			player.setTime(new Date());
			deviceService.update(player);
			playerCaches.storeStatus(player.getId(), message.getStatus());
		} catch (Exception e) {
			LOGGER.debug("Error handle media player message: " + payload);
			LOGGER.error("Error handle media player message: " + e.getMessage(), e);
		}
	}
	
}
