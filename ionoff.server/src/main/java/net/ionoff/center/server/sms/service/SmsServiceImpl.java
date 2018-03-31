package net.ionoff.center.server.sms.service;

import org.apache.log4j.Logger;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import com.google.gson.Gson;

import net.ionoff.center.server.config.AppConfig;
import net.ionoff.center.server.util.CommonUtil;
import net.ionoff.center.shared.dto.MessageDto;

public class SmsServiceImpl implements SmsService {

	private static Logger LOGGER = Logger.getLogger(SmsServiceImpl.class.getName());

	private RestTemplate restTemplate;
	private Gson gson;
	
	public SmsServiceImpl() {
		gson = new Gson();
		restTemplate = new RestTemplate();
	}

	@Override
	public MessageDto sendSms(String language, String[] subscribers, String sensor, boolean detected, String dateTime) {
		String subcribersStr = CommonUtil.toString(subscribers);
		LOGGER.info("Sending SMS to " + subcribersStr + ". Sensor: " + sensor + " - Detected: " + detected);
		
		String url = AppConfig.getInstance().NOTIFY_SERVICE_URL + "/sms/sensor";
		url = url + "?language=" + language;
		url = url + "&subscribers=" + subcribersStr;
		url = url + "&sensor=" + sensor;
		url = url + "&dateTime=" + dateTime;
		url = url + "&detected=" + detected;
		
		try {
			ResponseEntity<String> responseEntity = restTemplate.getForEntity(url, String.class);
			return gson.fromJson(responseEntity.getBody(), MessageDto.class);
		}
		catch (Exception e) {
			LOGGER.error("Error sending SMS: " + url);
			LOGGER.error(e.getMessage(), e);
			MessageDto messageDto = new MessageDto();
			messageDto.setStatus(500);
			messageDto.setMessage(e.getMessage());
			return messageDto;
		}
	}
}
