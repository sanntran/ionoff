package net.ionoff.notify;

import java.io.UnsupportedEncodingException;

import javax.mail.MessagingException;
import javax.mail.internet.AddressException;

public interface IMailService {
	String sendGmailEmail(String subscriber[], String subject, String content) 
			throws AddressException, MessagingException, UnsupportedEncodingException;

	String sendSmtpEmail(String[] subscribers, String subject, String content)
			throws MessagingException, UnsupportedEncodingException;
}
