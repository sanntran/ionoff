package net.ionoff.center.server.notify.connector;

import net.ionoff.center.shared.dto.MessageDto;

public interface INotificationConnector {

	MessageDto sendSms(String[] subscribers, String mesage);

	MessageDto sendEmail(String[] subscribers, String mesage);

}
