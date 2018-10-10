package net.ionoff.center.server.message.handler;

import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.server.entity.ModeSensorUser;
import net.ionoff.center.server.notify.INotificationService;
import org.apache.log4j.Logger;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

class ModeSensorUserActivator extends Thread {
	
	private static Logger LOGGER = Logger.getLogger(ModeSensorUserActivator.class.getName());
	
	private final String threadName;
	private final ModeSensor modeSensor;
	private final INotificationService notificationService;

	private static final SimpleDateFormat DATETIME_FORMAT = new SimpleDateFormat("dd/MM/yyyy HH:mm");
	
	ModeSensorUserActivator(ModeSensor modeSensor, INotificationService notificationService) {
		this.modeSensor = modeSensor;
		this.notificationService = notificationService;
		threadName = "Thread of " + modeSensor.toString();
	}
	
	@Override
	public void run() {
		if (modeSensor.hasUser()) {
			List<ModeSensorUser> subscribers = new ArrayList<ModeSensorUser>();
			for (ModeSensorUser sensorUser : modeSensor.getUsers()) {
				subscribers.add(sensorUser);
			}
			if (!subscribers.isEmpty()) {
				List<String> subscriberPhones = new ArrayList<>();
				List<String> subscriberMails = new ArrayList<>();
				for (ModeSensorUser subscriber : subscribers) {
					if (subscriber.izSendSms()) {
						subscriberPhones.add(subscriber.getUser().getPhoneNo());
					}
					if (subscriber.izSendEmail()) {
						subscriberMails.add(subscriber.getUser().getEmail());
					}
				}
				if (subscriberPhones.isEmpty()) {
					LOGGER.info(threadName + " sends SMS to no one");
				}
				else {
					notificationService.sendSms(subscriberPhones.toArray(new String[subscriberPhones.size()]), createMessage());
					LOGGER.info(threadName + " has finised sending SMS");
				}
				
				if (subscriberMails.isEmpty()) {
					LOGGER.info(threadName + " sends email to no one");
				}
				else {
					notificationService.sendEmail(subscriberMails.toArray(new String[subscriberMails.size()]), createMessage());
					LOGGER.info(threadName + " has finised sending email");
				}
			}
		}
	}

	private String createMessage() {
		String now = DATETIME_FORMAT.format(new Date());
		return new StringBuilder().append(modeSensor.getSensor().getNameId())
				.append(" (").append(now).append("): ").append(modeSensor.getMessage()).toString();
	}
}
