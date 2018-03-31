package net.ionoff.service;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Properties;

import org.apache.log4j.Logger;

public class UpdatorConfig {
	private static final Logger LOGGER = Logger.getLogger(UpdatorConfig.class.getName());

	private static UpdatorConfig INSTANCE = null;

	public final String UPDATE_SITE;
	public final String UPDATE_FOLDER;
	public final String LATESTVERSION_FILE;
	public final String LATESTVERSION_FILE_URL;
	public final String LOCAL_UPDATE_FOLDER;
	public final String LOCAL_LATESTVERSION_FILE;
	public final String[] DEPLOYED_WARS;
	public final String WEBAPP_FOLDER;
	public final String DB_CONNECTION_URL;
	public final String DB_CONNECTION_USER;
	public final String DB_CONNECTION_PASS;
	public final String DB_CONNECTION_DRIVER;

	public UpdatorConfig() {

		Properties prop = new Properties();
		InputStream stream = null;
		InputStreamReader isr = null;
		try {
			stream = Thread.currentThread().getContextClassLoader().getResourceAsStream("config.properties");
			isr = new InputStreamReader(stream, "UTF-8");
			prop.load(isr);
		} catch (IOException e) {
			LOGGER.error(e.getMessage(), e);
		}
		UPDATE_SITE = prop.getProperty("update_site");
		String updateFolder = prop.getProperty("update_folder");
		String[] s = updateFolder.split("/");
		StringBuilder builder = new StringBuilder();
		for (int i = 0; i < s.length; i++) {
			builder.append(s[i]);
			if (i != s.length - 1) {
				builder.append(File.separator);
			}
		}
		UPDATE_FOLDER = builder.toString();
		LOCAL_UPDATE_FOLDER = System.getProperty("catalina.base") + File.separator + UPDATE_FOLDER;
		LATESTVERSION_FILE = prop.getProperty("latestversion_file");
		LATESTVERSION_FILE_URL = UPDATE_SITE + "/" + LATESTVERSION_FILE;
		LOCAL_LATESTVERSION_FILE = LOCAL_UPDATE_FOLDER + File.separator + LATESTVERSION_FILE;
		String deployedWars = prop.getProperty("deployed_wars");
		if (deployedWars != null) {
			DEPLOYED_WARS = deployedWars.split(",");
		} else {
			DEPLOYED_WARS = new String[] {};
		}

		WEBAPP_FOLDER = System.getProperty("catalina.base") + File.separator + "webapps";
		DB_CONNECTION_URL = prop.getProperty("db_connection_url");
		DB_CONNECTION_USER = prop.getProperty("db_connection_user");
		DB_CONNECTION_PASS = prop.getProperty("db_connection_pass");
		DB_CONNECTION_DRIVER = prop.getProperty("db_connection_driver");

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

	public static UpdatorConfig getInstance() {
		if (INSTANCE == null) {
			INSTANCE = new UpdatorConfig();
		}
		return INSTANCE;
	}
}
