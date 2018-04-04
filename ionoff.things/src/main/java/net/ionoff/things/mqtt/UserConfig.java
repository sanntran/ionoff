package net.ionoff.things.mqtt;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Properties;

public class UserConfig {
		
	public static final String MQTT_URL;
	public static final String MQTT_USER;
	public static final String MQTT_PASSWORD;
	public static final String MQTT_TOPIC_CMD_FORMAT;
	public static final String MQTT_TOPIC_MSG_FORMAT;
	
	static {
		final Properties userConfig = new Properties();
		try {
			// load a properties file
			File currentDirUserConfigFile = new File(getCurrentDir() + File.separator + "config.properties");
			InputStream appConfigInput = new FileInputStream(currentDirUserConfigFile.getAbsolutePath());

			userConfig.load(appConfigInput);
		}
		catch (final IOException ex) {
			ex.printStackTrace();
		}		
		MQTT_URL = userConfig.getProperty("MQTT_URL");
		MQTT_USER = userConfig.getProperty("MQTT_USER");
		MQTT_PASSWORD = userConfig.getProperty("MQTT_PASSWORD");
		MQTT_TOPIC_CMD_FORMAT = userConfig.getProperty("MQTT_TOPIC_CMD_FORMAT");
		MQTT_TOPIC_MSG_FORMAT = userConfig.getProperty("MQTT_TOPIC_MSG_FORMAT");
	}
	

	public static String getCurrentDir() {
		String dir = System.getProperty("user.dir").replaceAll("/target/classes", "");
		System.out.println("Current dir: " + dir);
		URL source = UserConfig.class.getProtectionDomain().getCodeSource().getLocation();
		if (dir.equals(source.getPath())) {
			return new File(dir).getParent();
		}
		return dir;
	}
	
	public static void main(String[] args) {
		getCurrentDir();
	}
}
