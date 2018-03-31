package net.ionoff.center.server.email.service;

import net.ionoff.center.shared.dto.MessageDto;

public interface EmailService {
	MessageDto sendEmail(String language, String[] subscribers, String sensor, boolean detected, String dateTime);
}
