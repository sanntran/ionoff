package net.ionoff.center.server.sms.service;

import net.ionoff.center.shared.dto.MessageDto;

public interface SmsService {
	MessageDto sendSms(String[] subscribers, String message);
}
