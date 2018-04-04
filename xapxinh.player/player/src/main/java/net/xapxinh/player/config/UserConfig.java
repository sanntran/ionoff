package net.xapxinh.player.config;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import org.apache.log4j.Logger;

public class UserConfig {
	
	private static final Logger LOGGER = Logger.getLogger(UserConfig.class.getName());
	
	private static UserConfig instance;
	public final String HARDWARE_ID;
	public final String SERVER_HOST;
	public Integer SERVER_PORT;
	public final String LOGIN_PASSWORD;
	public final String ROOT_BROWSE_DIR;

	public UserConfig() {
		
		final Properties userConfig = new Properties();

		try {
			// load a properties file
			String currentDir = AppUtil.getCurrentDir();
			InputStream appConfigInput = new FileInputStream(currentDir + File.separator + "user-config.properties");
			userConfig.load(appConfigInput);
		}
		catch (final IOException ex) {
			LOGGER.error(ex.getMessage(), ex);
		}
		SERVER_HOST = userConfig.getProperty("server_host");
		HARDWARE_ID = userConfig.getProperty("hardware_id");
		try {
			SERVER_PORT = Integer.parseInt(userConfig.getProperty("server_port"));
		}
		catch (Exception e) {
			SERVER_PORT = null;
			LOGGER.info("User config SERVER_PORT is not set. Use built-in config value");
		}
		
		LOGIN_PASSWORD = userConfig.getProperty("login_password");
		
		String rootBrowseDir = userConfig.getProperty("root_browse_dir");
		if (rootBrowseDir == null || rootBrowseDir.trim().isEmpty()) {
			ROOT_BROWSE_DIR = AppUtil.getUserHomeDir() + File.separator + "Media";
		}
		else {
			ROOT_BROWSE_DIR =  rootBrowseDir;
		}
		File rootBrowseDirFile = new File(ROOT_BROWSE_DIR);
		if (!rootBrowseDirFile.exists()) {
			AppUtil.mkDir(rootBrowseDirFile);
		}
	}

	public static UserConfig getInstance() {
		if (instance == null) {
			instance = new UserConfig();
		}
		return instance;
	}

	public String getAlbumFolder() {
		return ROOT_BROWSE_DIR + File.separator + ".albums";
	}
}
