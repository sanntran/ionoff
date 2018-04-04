package net.xapxinh.player.config;

import java.io.File;
import java.io.IOException;
import java.util.Properties;

import org.apache.log4j.Logger;

public class AppConfig {
	
	private static final Logger LOGGER = Logger.getLogger(AppConfig.class.getName());
	
	private static AppConfig instance;
	
	public final String CENTER_SERVER_HOST;
	public int CENTER_SERVER_PORT;
	public final String DATA_SERVER_URL;
	public String VLC_LIBRARY_PATH;
	public final String UPDATE_SITE;
	public boolean INTERVAL_UPDATE;

	public AppConfig() {
		
		final Properties appProperties = new Properties();
		try {
			// load a properties file
			appProperties.load(Thread.currentThread().getContextClassLoader().getResourceAsStream("config.properties"));
		}
		catch (final IOException ex) {
			LOGGER.error(ex.getMessage(), ex);
			System.exit(0);
		}
		DATA_SERVER_URL = appProperties.getProperty("data_server_url");
		if (UserConfig.getInstance().SERVER_HOST != null) {
			CENTER_SERVER_HOST = UserConfig.getInstance().SERVER_HOST;
		}
		else {
			CENTER_SERVER_HOST = appProperties.getProperty("center_server_host");
		}
		if (UserConfig.getInstance().SERVER_PORT != null) {
			CENTER_SERVER_PORT = UserConfig.getInstance().SERVER_PORT;
		}
		else {
			try {
				CENTER_SERVER_PORT = Integer.parseInt(appProperties.getProperty("center_server_port"));
			}
			catch (Exception e) {
				CENTER_SERVER_PORT = 39994;
				LOGGER.info("CENTER_SERVER_PORT is not set. Use default value");
			}
		}
		UPDATE_SITE = appProperties.getProperty("update_site");
		VLC_LIBRARY_PATH = new File(AppUtil.getCurrentDir()) + File.separator + appProperties.getProperty("vlc_library");
		File vlcExe = new File(VLC_LIBRARY_PATH  + File.separator + "vlc.exe");
		if (!vlcExe.exists()) {
			VLC_LIBRARY_PATH = new File(AppUtil.getCurrentDir()).getParent() + File.separator + appProperties.getProperty("vlc_library");
		}
		try {
			INTERVAL_UPDATE = Boolean.parseBoolean(appProperties.getProperty("interval_update"));
		}
		catch (Exception e) {
			INTERVAL_UPDATE = false;
			LOGGER.info("INTERVAL_UPDATE is not set. Use default value");
		}
	}
	
	public static AppConfig getInstance() {
		if (instance == null) {
			instance = new AppConfig();
		}
		return instance;
	}
}
