package net.ionoff.player.config;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.log4j.Logger;

public class UserConfig {
	
	private static final Logger LOGGER = Logger.getLogger(UserConfig.class.getName());
	
	private static UserConfig INSTANCE;
	
	public String SERVER_HOST;
	public int SERVER_PORT;
	public String LOGIN_PASSWORD;
	public String ROOT_BROWSE_DIR;
	public String MPD_MUSIC_DIRECTORY;
	public String LICENSE_KEY;

	public static final String DEFAULT_SERVER_HOST = "localhost";
	public static final int DEFAULT_SERVER_PORT = 8118;
	public static final String DEFAULT_LOGIN_PASSWORD = "1234";
	public static final String DEFAULT_ROOT_BROWSE_DIR = "/home/ubuntu/music";
	public static final String DEFAULT_MPD_MUSIC_DIRECTORY = DEFAULT_ROOT_BROWSE_DIR;
	public static final String DEFAULT_LICENSE_KEY = AppUtil.getMacAddresses().get(0);

	public UserConfig() {
		final Properties userConfig = new Properties();
		try {
			// load a properties file
			File currentDirUserConfigFile = new File(AppUtil.getCurrentDir() + File.separator + "imp.properties");
			if (currentDirUserConfigFile.exists()) {
				InputStream appConfigInput = new FileInputStream(currentDirUserConfigFile.getAbsolutePath());
				userConfig.load(appConfigInput);
			}
			else {
				SERVER_HOST = "localhost";
				SERVER_PORT = 8118;
				LOGIN_PASSWORD = DEFAULT_LOGIN_PASSWORD;
				MPD_MUSIC_DIRECTORY = "/home/ubuntu/music";
				ROOT_BROWSE_DIR = MPD_MUSIC_DIRECTORY;
				return;
			}
		}
		catch (final IOException ex) {
			LOGGER.error(ex.getMessage(), ex);
		}
		LICENSE_KEY = userConfig.getProperty("license_key");
		SERVER_HOST = userConfig.getProperty("server_host");
		try {
			SERVER_PORT = Integer.parseInt(userConfig.getProperty("server_port"));
		}
		catch (Exception e) {
			SERVER_PORT = DEFAULT_SERVER_PORT;
			LOGGER.info("SERVER_PORT is not set. Use DEFAULT_SERVER_PORT " + DEFAULT_SERVER_PORT);
		}
		LOGIN_PASSWORD = userConfig.getProperty("login_password");
		MPD_MUSIC_DIRECTORY = userConfig.getProperty("mpd_music_directory");
		
		File mpdMusicDirectory = new File(MPD_MUSIC_DIRECTORY);
		if (!mpdMusicDirectory.exists()) {
			AppUtil.mkDir(mpdMusicDirectory);
		}
		ROOT_BROWSE_DIR = MPD_MUSIC_DIRECTORY;
	}

	public synchronized static UserConfig getInstance() {
		if (INSTANCE == null) {
			INSTANCE = new UserConfig();
		}
		return INSTANCE;
	}

	public String getAlbumFolder() {
		return ROOT_BROWSE_DIR + "/.albums";
	}
}
