package net.ionoff.center.server.notify;

import net.ionoff.center.server.notify.connector.INotificationConnector;
import net.ionoff.center.server.util.CommonUtil;
import net.ionoff.center.shared.dto.MessageDto;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;

@Component
@EnableAsync
public class NotificationServiceImpl implements INotificationService {

	private static Logger LOGGER = Logger.getLogger(NotificationServiceImpl.class.getName());

	@Autowired
	private INotificationConnector notificationConnector;

	@Override
	public void sendSms(String[] subscribers, String message) {
		try {
			MessageDto response = notificationConnector.sendSms(subscribers, message);
			LOGGER.info("Notification service response: " + response.getMessage());
		}
		catch (Exception e) {
			String subcribersStr = CommonUtil.toString(subscribers);
			LOGGER.error("Error sending SMS to " + subcribersStr);
			LOGGER.error(e.getMessage(), e);
		}
	}


	@Override
	public void sendEmail(String[] subscribers, String message) {
		try {
			MessageDto response = notificationConnector.sendEmail(subscribers, message);
			LOGGER.info("Notification service response: " + response.getMessage());
		}
		catch (Exception e) {
			String subcribersStr = CommonUtil.toString(subscribers);
			LOGGER.error("Error sending email to: " + subcribersStr);
			LOGGER.error(e.getMessage(), e);
		}
	}
}
