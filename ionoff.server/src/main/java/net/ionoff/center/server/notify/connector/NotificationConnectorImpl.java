package net.ionoff.center.server.notify.connector;

import com.google.gson.Gson;
import net.ionoff.center.server.mediaplayer.connector.MediaPlayerRequestExceptionHandler;
import net.ionoff.center.server.util.CommonUtil;
import net.ionoff.center.server.wsclient.RestTemplateFactory;
import net.ionoff.center.server.wsclient.RestTemplateRequestIntercepter;
import net.ionoff.center.shared.dto.MessageDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestTemplate;

import java.util.Arrays;

@Component
@EnableAsync
public class NotificationConnectorImpl implements INotificationConnector {

	private static final Logger LOGGER = LoggerFactory.getLogger(NotificationConnectorImpl.class.getName());
	private static final Gson GSON = new Gson();

	private RestTemplate restTemplate;
	private Gson gson;

	@Value("${service.notify.url}")
	private String  notifyServiceUrl;

	public NotificationConnectorImpl() {
		gson = new Gson();
		restTemplate = RestTemplateFactory.buildRestTemplate(
				new NotificationRequestExceptionHandler(),
				Arrays.asList(new RestTemplateRequestIntercepter()));
	}

	@Async
	@Override
	public MessageDto sendSms(String[] subscribers, String message) {
		String subcribersStr = CommonUtil.toString(subscribers);
		LOGGER.info("Sending SMS to " + subcribersStr);
		String url = notifyServiceUrl + "/sms/sensor";
		url = url + "?subscribers=" + subcribersStr;
		ResponseEntity<String> responseEntity = restTemplate.postForEntity(url, message, String.class);
		return gson.fromJson(responseEntity.getBody(), MessageDto.class);
	}

	@Async
	@Override
	public MessageDto sendEmail(String[] subscribers, String message) {
		String subcribersStr = CommonUtil.toString(subscribers);
		LOGGER.info("Sending email to " + subcribersStr);
		String url = notifyServiceUrl + "/email";
		url = url + "?subscribers=" + subcribersStr;
		ResponseEntity<String> responseEntity = restTemplate.postForEntity(url, message, String.class);
		return gson.fromJson(responseEntity.getBody(), MessageDto.class);
	}
}
