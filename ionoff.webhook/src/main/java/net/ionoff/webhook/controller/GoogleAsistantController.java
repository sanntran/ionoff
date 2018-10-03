package net.ionoff.webhook.controller;

import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import com.fasterxml.jackson.databind.util.JSONPObject;
import net.ionoff.webhook.dto.WebhookRequest;
import net.ionoff.webhook.dto.WebhookResponse;
import net.ionoff.webhook.model.Center;
import net.ionoff.webhook.service.CenterService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import org.springframework.web.client.RestTemplate;

import javax.servlet.http.HttpServletRequest;

@RestController
@RequestMapping(value = "/googleassistants")
public class GoogleAsistantController {
	
	private static final Logger logger = LoggerFactory.getLogger(GoogleAsistantController.class);
	private static final String NOT_FOUND = "NotFound";

	private static final SimpleDateFormat TIME_FORMATTER = new SimpleDateFormat("HH:mm");
	private static final SimpleDateFormat DATE_FORMATTER = new SimpleDateFormat("dd/MM/yyyy");


	@Autowired
	CenterService centerService;

	@RequestMapping(value = "/{id}",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public WebhookResponse fulfillText(@PathVariable(value = "id") String id,
									   @RequestBody WebhookRequest webhookRequest, HttpServletRequest request) {
		WebhookResponse resp = new WebhookResponse();
		resp.setFulfillmentText("Ok the light is turn on");

		String query = webhookRequest.getQueryResult().getQueryText();

		/*
		// 	nếu query là play kịch bản,
		//
			Lấy tên kịch bản
			Lấy tên vùng



			Lấy center server ip port

			Gởi request tới center server


		*/

		Optional<Center> center = centerService.getById(id);
		center.get().getIp();


		RestTemplate restTemplate = new RestTemplate();

		String centerUrl = "http://ionosd.sjkdfkd.com/iserver/api/scenes/play";
		Map<String, String> body = new HashMap<>();
		body.put("sceneName", "Đi ngủ");
		body.put("zoneName", "Phòng Ngủ");



		JSONPObject json = restTemplate.postForObject(centerUrl, body, JSONPObject.class);

		System.out.println(query);

		return resp;
	}

}
