package net.ionoff.notify.email;

import java.io.UnsupportedEncodingException;
import java.util.HashMap;

import javax.mail.MessagingException;

import org.apache.log4j.Logger;

import net.ionoff.notify.CommonUtil;
import net.ionoff.notify.IMailService;
import net.ionoff.notify.NotifyConfig;

public class EmailServiceImpl implements IMailService {

	private static Logger LOGGER = Logger.getLogger(EmailServiceImpl.class.getName());

	private final EmailApi emailApi;

	public EmailServiceImpl(EmailApi emailApi) {
		this.emailApi = emailApi;
	}

	@Override
	public String sendSmtpEmail(String subscribers[], String subject, String content) throws MessagingException, UnsupportedEncodingException {
		LOGGER.info("Sending email to: " + subscribers);
		LOGGER.info("Sending email message: " + content);

		HashMap<String, String> params = getMailParamsMap(subject, content);
		params.put("smtp_mail_host", NotifyConfig.getInstance().SMTP_MAIL_HOST);
		params.put("smtp_mail_auth_user", NotifyConfig.getInstance().SMTP_MAIL_AUTH_USER);
		params.put("smtp_mail_auth_pwd", NotifyConfig.getInstance().SMTP_MAIL_AUTH_PWD);
		params.put("smtp_mail_from_address", NotifyConfig.getInstance().SMTP_MAIL_FROM_ADDRESS);
		String result = emailApi.sendSmtpEmail(params, subscribers);
		LOGGER.info("Result: " + result);
		return result;
	}
	
	@Override
	public String sendGmailEmail(String subscribers[], String subject, String content) throws MessagingException, UnsupportedEncodingException {
		LOGGER.info("Sending email to " + CommonUtil.toString(subscribers) + "\n - Email message: " + content);

		HashMap<String, String> params = getMailParamsMap(subject, content);
		
		params.put("gmail_auth_user", NotifyConfig.getInstance().GMAIL_AUTH_USER);
		params.put("gmail_auth_pwd", NotifyConfig.getInstance().GMAIL_AUTH_PWD);
		
		String result = emailApi.sendGmailEmail(params, subscribers);
		LOGGER.info("Result: " + result);
		return result;
	}

	private HashMap<String, String> getMailParamsMap(String subject, String message) {
		HashMap<String, String> params = new HashMap<String, String>();
		params.put("mail_from_name", NotifyConfig.getInstance().MAIL_FROM_NAME);
		params.put("subject", subject);
		params.put("message", message);

		return params;
	}
}
