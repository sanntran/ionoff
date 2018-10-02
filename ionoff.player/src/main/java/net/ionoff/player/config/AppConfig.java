package net.ionoff.player.config;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Properties;

import org.apache.log4j.Logger;

public class AppConfig {
	
	private static final Logger LOGGER = Logger.getLogger(AppConfig.class.getName());

	public static final AppConfig INSTANCE = new AppConfig();
	
	public final String UPDATE_SITE;
	public final String MEDIA_SERVER_URL;
	public final String MPD_HOST;
	public final int MPD_PORT;
	public final boolean INTERVAL_UPDATE;
	public final String VERSION;
	public final String APP_DIR;
	
	private AppConfig() {
		
		final Properties appProperties = new Properties();
		try {
			// load a properties file
			appProperties.load(Thread.currentThread().getContextClassLoader().getResourceAsStream("application.properties"));
		}
		catch (final IOException ex) {
			LOGGER.error("Error reading application.properties " + ex.getMessage(), ex);
		}

		UPDATE_SITE = appProperties.getProperty("update_site");
		MEDIA_SERVER_URL = appProperties.getProperty("media_server_url");
		MPD_HOST = appProperties.getProperty("mpd_host");
		MPD_PORT = getMpdPortProperty(appProperties);
		INTERVAL_UPDATE = getIntervalUpdateProperty(appProperties);
		VERSION = appProperties.getProperty("version");
		APP_DIR = getAppDir();
	}

	private boolean getIntervalUpdateProperty(Properties appProperties) {
		try {
			return Boolean.parseBoolean(appProperties.getProperty("interval_update"));
		}
		catch (Exception e) {
			LOGGER.info("INTERVAL_UPDATE is not set. Use default value");
			return true;
		}
	}

	private int getMpdPortProperty(Properties appProperties) {
		try {
			return Integer.parseInt(appProperties.getProperty("mpd_port"));
		}
		catch (Exception e) {
			LOGGER.info("MPD_PORT is not set. Use default value");
			return 6600;
		}
	}

	private static String getAppDir() {
		String dir = ClassLoader.getSystemClassLoader().getResource(".").getPath();
		URL source = AppConfig.class.getProtectionDomain().getCodeSource().getLocation();
		if (dir.equals(source.getPath())) {
			return new File(dir).getParent();
		}
		return dir;
	}

}
