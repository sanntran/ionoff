package net.ionoff.notify;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Properties;

import org.apache.log4j.Logger;

public class NotifyConfig {
	private static final Logger LOGGER = Logger.getLogger(NotifyConfig.class.getName());

	private static NotifyConfig INSTANCE = null;
	
	public final String SMS_GATEWAY_URL;
	public final String SMS_GATEWAY_API_KEY;
	public final String SMS_GATEWAY_SECRET_KEY;
	
	public final String MAIL_FROM_NAME;
	
	public final String GMAIL_AUTH_USER;
	public final String GMAIL_AUTH_PWD;

	public final String SMTP_MAIL_HOST;
	public final String SMTP_MAIL_AUTH_USER;
	public final String SMTP_MAIL_AUTH_PWD;
	public final String SMTP_MAIL_FROM_ADDRESS;

	public NotifyConfig() {

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
		SMS_GATEWAY_URL = prop.getProperty("SMS_GATEWAY_URL");
		SMS_GATEWAY_API_KEY = prop.getProperty("SMS_GATEWAY_API_KEY");
		SMS_GATEWAY_SECRET_KEY = prop.getProperty("SMS_GATEWAY_SECRET_KEY");
		
		MAIL_FROM_NAME = prop.getProperty("MAIL_FROM_NAME");
		
		GMAIL_AUTH_USER = prop.getProperty("GMAIL_AUTH_USER");
		GMAIL_AUTH_PWD = prop.getProperty("GMAIL_AUTH_PWD");

		SMTP_MAIL_HOST = prop.getProperty("SMTP_MAIL_HOST");
		SMTP_MAIL_AUTH_USER = prop.getProperty("SMTP_MAIL_AUTH_USER");
		SMTP_MAIL_AUTH_PWD = prop.getProperty("SMTP_MAIL_AUTH_PWD");
		SMTP_MAIL_FROM_ADDRESS = prop.getProperty("SMTP_MAIL_FROM_ADDRESS");
		
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

	public static NotifyConfig getInstance() {
		if (INSTANCE == null) {
			INSTANCE = new NotifyConfig();
		}
		return INSTANCE;
	}
	

	public String getSensorNotificationEmailForm(String language) {
		if ("vi".equals(language)) {
			String content = "<p>" 
					+ "<strong>" 
					+ "$SENSOR $BOOLEAN phát hiện người lúc $DATETIME" 
					+ "</strong>" 
					+ "</p>";
			return content;
		}
		String content = "<p>" 
				+ "<strong>" 
				+ "$SENSOR $BOOLEAN detected human at $DATETIME" 
				+ "</strong>"
				+ "</p>";
		return content;
	}
	
	public String getSensorNotificationEmailMessage(String language, String sensor, boolean detected, String dateTime) {
		String message = getSensorNotificationEmailForm(language);
		message = message.replaceFirst("\\$SENSOR", sensor);
		if ("vi".equals(language)) {
			message = message.replaceFirst("\\$BOOLEAN", detected? "" : "KHÔNG");
		}
		else {
			message = message.replaceFirst("\\$BOOLEAN", detected? "" : "NOT");
		}
		message = message.replaceFirst("\\$DATETIME", dateTime);
		return message;
	}
	
	public String getSensorNotificationSmsForm(String language) {
		if ("vi".equals(language)) {
			return "$SENSOR $BOOLEAN phat hien nguoi luc $DATETIME";
		}
		return "$SENSOR $BOOLEAN detected human at $DATETIME";
	}
	
	public String getSensorNotificationSmsMessage(String language, String sensor, boolean detected, String dateTime) {
		String message = getSensorNotificationSmsForm(language);
		message = message.replaceFirst("\\$SENSOR", sensor);
		if ("vi".equals(language)) {
			message = message.replaceFirst("\\$BOOLEAN", detected? "" : "KHONG");
		}
		else {
			message = message.replaceFirst("\\$BOOLEAN", detected? "" : "NOT");
		}
		
		message = message.replaceFirst("\\$DATETIME", dateTime);
		return message;
	}
}
