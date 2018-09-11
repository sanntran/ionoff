package net.ionoff.center.server.notify;

import net.ionoff.center.shared.dto.MessageDto;

public interface SmsService {
	MessageDto sendSms(String[] subscribers, String message);
}
