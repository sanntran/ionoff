package net.ionoff.center.server.mediaplayer;

import net.ionoff.center.server.persistence.service.IDeviceService;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;

public class MediaPlayerHandler {

	private static final Logger LOGGER = Logger.getLogger(MediaPlayerHandler.class.getName());

	@Autowired
	private IDeviceService deviceService;

	public void onMessageArrived(String payload) {

	}
	
}
