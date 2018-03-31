package net.ionoff.notify.email;

import java.io.UnsupportedEncodingException;
import java.util.Date;
import java.util.HashMap;
import java.util.Properties;

import javax.mail.Authenticator;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

public class EmailApiImpl implements EmailApi {

	@Override
	public String sendSmtpEmail(HashMap<String, String> params, String recipients[]) throws UnsupportedEncodingException,
			MessagingException {

		boolean debug = false;
		// Set the host smtp address
		Properties props = new Properties();
		props.put("mail.smtp.host", params.get("smtp_mail_host"));
		props.put("mail.smtp.auth", "true");
		Authenticator auth = new SMTPAuthenticator(params.get("smtp_mail_auth_user"), params.get("smtp_mail_auth_pwd"));
		Session session = Session.getDefaultInstance(props, auth);
		session.setDebug(debug);
		// create a message
		Message msg = new MimeMessage(session);
		// set the from and to address
		InternetAddress addressFrom = new InternetAddress(params.get("smtp_mail_from_address"), params.get("mail_from_name"));
		msg.setFrom(addressFrom);

		InternetAddress[] addressTo = new InternetAddress[recipients.length];
		for (int i = 0; i < recipients.length; i++) {
			addressTo[i] = new InternetAddress(recipients[i]);
		}
		msg.setRecipients(Message.RecipientType.TO, addressTo);

		msg.setSubject(params.get("subject"));
		msg.setHeader("Content-Type", "text/html; charset=\"utf-8\"");
		msg.setContent(params.get("message"), "text/html; charset=utf-8");
		Transport.send(msg);

		return "Email have been sent succesfully!";
	}

	private class SMTPAuthenticator extends javax.mail.Authenticator {
		private String smtpUsername;
		private String smtpPassword;

		private SMTPAuthenticator(String username, String password) {
			smtpUsername = username;
			smtpPassword = password;
		}

		@Override
		public PasswordAuthentication getPasswordAuthentication() {
			return new PasswordAuthentication(smtpUsername, smtpPassword);
		}
	}
	
	@Override
	public String sendGmailEmail(final HashMap<String, String> params, String recipients[]) throws AddressException,
			MessagingException {
		
		Properties props = new Properties();
		props.put("mail.smtp.host", "smtp.gmail.com");
		props.put("mail.smtp.auth", "true");
		props.put("mail.debug", "true");
		props.put("mail.smtp.starttls.enable", "true");
		props.put("mail.smtp.port", "465");
		props.put("mail.smtp.socketFactory.port", "465");
		props.put("mail.smtp.socketFactory.class", "javax.net.ssl.SSLSocketFactory");
		props.put("mail.smtp.socketFactory.fallback", "false");

		Session mailSession = Session.getInstance(props, new javax.mail.Authenticator() {
			@Override
			protected PasswordAuthentication getPasswordAuthentication() {
				return new PasswordAuthentication(params.get("gmail_auth_user"), params.get("gmail_auth_pwd"));
			}
		});

		mailSession.setDebug(true); // Enable the debug mode

		Message msg = new MimeMessage(mailSession);
		// --[ Set the FROM, TO, DATE and SUBJECT fields
		msg.setFrom(new InternetAddress(params.get("mail_from_name")));

		InternetAddress[] addressTo = new InternetAddress[recipients.length];
		for (int i = 0; i < recipients.length; i++) {
			addressTo[i] = new InternetAddress(recipients[i]);
		}
		msg.setRecipients(Message.RecipientType.TO, addressTo);

		msg.setSentDate(new Date());
		msg.setSubject(params.get("subject"));

		// --[ Create the body of the mail
		// msg.setText( "Hello from my first e-mail sent with JavaMail" );
		msg.setHeader("Content-Type", "text/html; charset=\"utf-8\"");
		msg.setContent(params.get("message"), "text/html; charset=utf-8");

		// --[ Ask the Transport class to send our mail message
		Transport.send(msg);

		return "Email have been sent succesfully!";
	}
}