package net.ionoff.webhook.controller;

import java.text.SimpleDateFormat;

import net.ionoff.webhook.dto.WebhookRequest;
import net.ionoff.webhook.dto.WebhookResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;

@RestController
@RequestMapping(value = "/googleassistants")
public class GoogleAsistantController {
	
	private static final Logger logger = LoggerFactory.getLogger(GoogleAsistantController.class);
	private static final String NOT_FOUND = "NotFound";

	private static final SimpleDateFormat TIME_FORMATTER = new SimpleDateFormat("HH:mm");
	private static final SimpleDateFormat DATE_FORMATTER = new SimpleDateFormat("dd/MM/yyyy");


	@RequestMapping(value = "/{id}",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public WebhookResponse fulfillText(@RequestBody WebhookRequest webhookRequest, HttpServletRequest request) {
		WebhookResponse resp = new WebhookResponse();
		resp.setFulfillmentText("Ok the light is turn on");

		String query = webhookRequest.getQueryResult().getQueryText();
		System.out.println(query);

		return resp;
	}

}
