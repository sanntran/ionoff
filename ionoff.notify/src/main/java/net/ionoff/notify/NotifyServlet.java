package net.ionoff.notify;

import java.io.IOException;

import javax.mail.MessagingException;
import javax.mail.internet.AddressException;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;

import com.google.gson.Gson;

import net.ionoff.notify.email.EmailApiImpl;
import net.ionoff.notify.email.EmailServiceImpl;
import net.ionoff.notify.esms.SmsServiceImpl;

public class NotifyServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;
	
	private static final Logger LOGGER = Logger.getLogger(NotifyServlet.class.getName());
	private static final Gson GSON = new Gson();

	private IMailService mailService;
	private ISmsService smsService;
	
	@Override
	public void init() {
		mailService = new EmailServiceImpl(new EmailApiImpl());
		smsService = new SmsServiceImpl();
	}

	/**
	 * @see HttpServlet#doGet(HttpServletRequest request, HttpServletResponse
	 *      response)
	 */
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		String pathInfo = request.getPathInfo();
		try {
			if ("/sms/sensor".equals(pathInfo)) {
				MessageDto messageDto = sendSensorSmsNotification(request, response);
				response.getWriter().append(GSON.toJson(messageDto));
			}
			else if ("/email/sensor".equals(pathInfo)) {
				MessageDto messageDto = sendSensorEmailNotification(request, response);
				response.getWriter().append(GSON.toJson(messageDto));
			}
			else {
				response.setStatus(HttpServletResponse.SC_NOT_FOUND);
				MessageDto messageDto = new MessageDto();
				messageDto.setStatus(HttpServletResponse.SC_NOT_FOUND);
				messageDto.setMessage("Request is not handled");
				response.getWriter().append(GSON.toJson(messageDto));
			}
		}
		catch (Throwable t) {
			LOGGER.error(t.getMessage(), t);
			response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			MessageDto messageDto = new MessageDto();
			messageDto.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			messageDto.setMessage(t.getMessage());
			response.getWriter().append(GSON.toJson(messageDto));
		}
	}

	private MessageDto sendSensorEmailNotification(HttpServletRequest request, HttpServletResponse response) throws AddressException, MessagingException, IOException {
		String subscribers = request.getParameter("subscribers");
		String sensor = request.getParameter("sensor");
		String detected = request.getParameter("detected");
		String language = request.getParameter("language");
		String dateTime = request.getParameter("dateTime");
		MessageDto messageDto = validateRequestParams(subscribers, sensor, detected, dateTime);
		LOGGER.info("Request sending sensor email notification: ");
		LOGGER.info("------ Subscribers: " + subscribers);
		LOGGER.info("------ Sensor: " + sensor);
		LOGGER.info("------ Detected: " + detected);
		LOGGER.info("------ Language: " + language);
		LOGGER.info("------ DateTime: " + dateTime);
		if (messageDto != null) {
			response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
			return messageDto;
		}
		String[] subcriberArr = subscribers.split(",");
		boolean isDetected = false;
		if ("true".equals(detected)) {
			isDetected = true;
		}
		String message = mailService.sendGmailEmail(subcriberArr, "IOnOff Notification", 
				NotifyConfig.getInstance().getSensorNotificationEmailMessage(language, sensor, isDetected, dateTime));
		messageDto = new MessageDto();
		messageDto.setStatus(HttpServletResponse.SC_OK);
		messageDto.setMessage(message);
		return messageDto;
	}

	private MessageDto sendSensorSmsNotification(HttpServletRequest request, HttpServletResponse response) throws IOException {
		String subscribers = request.getParameter("subscribers");
		String sensor = request.getParameter("sensor");
		String detected = request.getParameter("detected");
		String language = request.getParameter("language");
		String dateTime = request.getParameter("dateTime");
		LOGGER.info("Request sending sensor sms notification: ");
		LOGGER.info("------ Subscribers: " + subscribers);
		LOGGER.info("------ Sensor: " + sensor);
		LOGGER.info("------ Detected: " + detected);
		LOGGER.info("------ Language: " + language);
		LOGGER.info("------ DateTime: " + dateTime);
		MessageDto messageDto = validateRequestParams(subscribers, sensor, detected, dateTime);
		if (messageDto != null) {
			response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
			return messageDto;
		}
		String[] subcriberArr = subscribers.split(",");
		boolean isDetected = false;
		if ("true".equals(detected)) {
			isDetected = true;
		}
		String message = smsService.sendSms(subcriberArr, 
				NotifyConfig.getInstance().getSensorNotificationSmsMessage(language, sensor, isDetected, dateTime));
		
		messageDto = new MessageDto();
		messageDto.setStatus(HttpServletResponse.SC_OK);
		messageDto.setMessage(message);
		return messageDto;
	}

	private MessageDto validateRequestParams(String subcribers, String sensor, String detected, String dateTime) {
		MessageDto messageDto = new MessageDto();
		if (subcribers == null || subcribers.isEmpty()) {
			messageDto.setStatus(HttpServletResponse.SC_BAD_REQUEST);
			messageDto.setMessage("Request parametter is not valid: subcribers");
			return messageDto;
		}
		if (sensor == null || sensor.isEmpty()) {
			messageDto.setStatus(HttpServletResponse.SC_BAD_REQUEST);
			messageDto.setMessage("Request parametter is not valid: sensor");
			return messageDto;
		}
		if (detected == null || detected.isEmpty()) {
			messageDto.setStatus(HttpServletResponse.SC_BAD_REQUEST);
			messageDto.setMessage("Request parametter is not valid: detected");
			return messageDto;
		}
		return null;
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		doGet(request, response);
	}
}
