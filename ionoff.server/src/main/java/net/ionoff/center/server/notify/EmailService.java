package net.ionoff.center.server.notify;

import net.ionoff.center.shared.dto.MessageDto;

public interface EmailService {
	MessageDto sendEmail(String[] subscribers, String message);
}
