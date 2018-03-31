package net.ionoff.notify.email;

import java.io.UnsupportedEncodingException;
import java.util.HashMap;

import javax.mail.MessagingException;
import javax.mail.internet.AddressException;

public interface EmailApi {

	public String sendSmtpEmail(HashMap<String, String> params, String recipients[]) throws UnsupportedEncodingException,
			MessagingException;

	public String sendGmailEmail(HashMap<String, String> params, String recipients[]) throws AddressException,
			MessagingException;
}