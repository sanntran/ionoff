package net.ionoff.center.server.notifier.handler;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.log4j.Logger;

import net.ionoff.center.server.email.service.EmailService;
import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.server.entity.ModeSensorUser;
import net.ionoff.center.server.sms.service.SmsService;

class ModeSensorUserActivator extends Thread {
	
	private static Logger LOGGER = Logger.getLogger(ModeSensorUserActivator.class.getName());
	
	private final String threadName;
	private final ModeSensor modeSensor;
	private final EmailService emailService;
	private final SmsService smsService;
	
	private static final SimpleDateFormat DATETIME_FORMAT = new SimpleDateFormat("dd/MM/yyyy HH:mm");
	
	ModeSensorUserActivator(ModeSensor modeSensor, EmailService emailService, SmsService smsService) {
		this.modeSensor = modeSensor;
		this.emailService = emailService;
		this.smsService = smsService;
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
					if (subscriber.isSendSms()) {
						subscriberPhones.add(subscriber.getUser().getPhoneNo());
					}
					if (subscriber.isSendEmail()) {
						subscriberMails.add(subscriber.getUser().getEmail());
					}
				}
				if (subscriberPhones.isEmpty()) {
					LOGGER.info(threadName + " sends SMS to no one");
				}
				else {
					smsService.sendSms(subscriberPhones.toArray(new String[subscriberPhones.size()]), createMessage());
					LOGGER.info(threadName + " has finised sending SMS");
				}
				
				if (subscriberMails.isEmpty()) {
					LOGGER.info(threadName + " sends email to no one");
				}
				else {
					emailService.sendEmail(subscriberMails.toArray(new String[subscriberMails.size()]), createMessage());
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
