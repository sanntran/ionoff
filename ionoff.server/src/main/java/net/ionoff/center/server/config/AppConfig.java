package net.ionoff.center.server.config;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Properties;

import org.apache.log4j.Logger;

public class AppConfig {
	
	static Logger LOGGER = Logger.getLogger(AppConfig.class.getName());

	private static AppConfig INSTANCE = null;
	
	public final int PLAYER_TCP_SERVER_PORT;
	public final String MEDIA_SERVICE_URL;
	public final String NOTIFY_SERVICE_URL;
	public final String UPDATOR_SERVICE_CONTEXT;
	public final String UPDATE_SITE;
	public final String UPDATE_FOLDER;
	public final String LATEST_VERSION_URL;
	public final String LATEST_VERSION_FILE;
	public final String CRON_FOLDER;
	
	public final String MQTT_USER;
	public final String MQTT_PASS;
	public final int MQTT_QOS;
	public final String MQTT_CLIENT_ID;
	public final String MQTT_BROKER_URL;
	public final String MQTT_TOPIC_IONOFF_NET;
	public final String MQTT_TOPIC_RELAY_DRIVER;
	public final String MQTT_TOPIC_WEIGH_SCALE;
	
	public AppConfig() {

		Properties prop = new Properties();
		InputStream stream = null;
		InputStreamReader isr = null;
		try {
			stream = Thread.currentThread().getContextClassLoader().getResourceAsStream("config.properties");
			isr = new InputStreamReader(stream, "UTF-8");
			prop.load(isr);
		}
		catch (IOException e) {
			LOGGER.error(e.getMessage(), e);
		}

		MEDIA_SERVICE_URL = prop.getProperty("MEDIA_SERVICE_URL");
		NOTIFY_SERVICE_URL = prop.getProperty("NOTIFY_SERVICE_URL");
		UPDATOR_SERVICE_CONTEXT  = prop.getProperty("UPDATOR_SERVICE_CONTEXT");
		UPDATE_SITE = prop.getProperty("UPDATE_SITE");
		UPDATE_FOLDER  = System.getProperty("catalina.base") 
							+ File.separator + prop.getProperty("UPDATE_FOLDER");
		
		LATEST_VERSION_FILE = System.getProperty("catalina.base") 
				+ File.separator + prop.getProperty("LATEST_VERSION_FILE");
		
		CRON_FOLDER = System.getProperty("catalina.base") 
				+ File.separator + prop.getProperty("CRON_FOLDER");
		
		LATEST_VERSION_URL = prop.getProperty("LATEST_VERSION_URL");
		
		MQTT_USER = prop.getProperty("MQTT_USER");
		MQTT_PASS = prop.getProperty("MQTT_PASS");
		MQTT_CLIENT_ID = prop.getProperty("MQTT_CLIENT_ID");
		MQTT_BROKER_URL = prop.getProperty("MQTT_BROKER_URL");
		MQTT_TOPIC_IONOFF_NET = prop.getProperty("MQTT_TOPIC_IONOFF_NET");
		MQTT_TOPIC_RELAY_DRIVER = prop.getProperty("MQTT_TOPIC_RELAY_DRIVER");
		MQTT_TOPIC_WEIGH_SCALE = prop.getProperty("MQTT_TOPIC_WEIGH_SCALE");
		
		int port = 39994;	
		try {
			port = Integer.parseInt(prop.getProperty("PLAYER_TCP_SERVER_PORT"));			
		}
		catch (NumberFormatException e) {
			LOGGER.info("PLAYER_TCP_SERVER_PORT is not set. Use default port " + port);
		}
		PLAYER_TCP_SERVER_PORT = port;
		
		int qos = 2;
		try {
			qos = Integer.parseInt(prop.getProperty("MQTT_QOS"));			
		}
		catch (NumberFormatException e) {
			LOGGER.info("MQTT_QOS is not set. Use defautl " + qos);
		}		
		MQTT_QOS = qos;
		
		try {
			if (stream != null) {
				stream.close();
			}
			if (isr != null) {
				isr.close();
			}
		} catch (IOException e) {
			LOGGER.error(e.getMessage(), e);
		}
	}

	public static AppConfig getInstance() {
		if (INSTANCE == null) {
			INSTANCE = new AppConfig();
		}
		return INSTANCE;
	}

	public static boolean isWinPlatform() {
		String os = System.getProperty("os.name");
		if (os.contains("Win")) {
			return true;
		}
		else
			return false;
	}

	public static boolean isLinuxPlatform() {
		String os = System.getProperty("os.name");
		if (os.contains("nux")) {
			return true;
		}
		else
			return false;
	}
}
