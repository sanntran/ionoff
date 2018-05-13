package net.ionoff.center.server.sms.service;

import java.nio.charset.Charset;

import org.apache.log4j.Logger;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.web.client.RestTemplate;

import com.google.gson.Gson;

import net.ionoff.center.server.config.AppConfig;
import net.ionoff.center.server.util.CommonUtil;
import net.ionoff.center.shared.dto.MessageDto;

@EnableAsync
public class SmsServiceImpl implements SmsService {

	private static Logger LOGGER = Logger.getLogger(SmsServiceImpl.class.getName());

	private RestTemplate restTemplate;
	private Gson gson;
	
	public SmsServiceImpl() {
		gson = new Gson();
		restTemplate = new RestTemplate();
		restTemplate.getMessageConverters()
        .add(0, new StringHttpMessageConverter(Charset.forName("UTF-8")));
	}
	
	@Async
	@Override
	public MessageDto sendSms(String[] subscribers, String message) {
		String subcribersStr = CommonUtil.toString(subscribers);
		LOGGER.info("Sending SMS to " + subcribersStr);
		
		String url = AppConfig.getInstance().NOTIFY_SERVICE_URL + "/sms/sensor";
		url = url + "?subscribers=" + subcribersStr;
		
		try {
			ResponseEntity<String> responseEntity = restTemplate.postForEntity(url, message, String.class);
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
