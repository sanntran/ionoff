package net.ionoff.webhook.controller;

import java.text.SimpleDateFormat;

import net.ionoff.webhook.dto.WebhookRequest;
import net.ionoff.webhook.dto.WebhookResponse;
import net.ionoff.webhook.model.Center;
import net.ionoff.webhook.repository.CenterRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import org.springframework.web.bind.annotation.RestController;

import javax.servlet.http.HttpServletRequest;

@RestController
@RequestMapping(value = "/webhook")
public class WebhookController {
	
	private static final Logger logger = LoggerFactory.getLogger(WebhookController.class);
	private static final String NOT_FOUND = "NotFound";
		
	private static final SimpleDateFormat TIME_FORMATTER = new SimpleDateFormat("HH:mm");
	private static final SimpleDateFormat DATE_FORMATTER = new SimpleDateFormat("dd/MM/yyyy");


	@Autowired
    CenterRepository repo;

	@RequestMapping(value = "/webhook",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public WebhookResponse getServerInfo(@RequestBody WebhookRequest webhookRequest, HttpServletRequest request) {
		WebhookResponse resp = new WebhookResponse();
		resp.setFulfillmentText("Ok the light is turn on");

		String query = webhookRequest.getQueryResult().getQueryText();
		System.out.println(query);
		repo.save(new Center());
		return resp;
	}

	@RequestMapping(value = "/ii",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public String getServerInfo(HttpServletRequest request) {
		repo.save(new Center());
		return "ajhsdjhasdk";
	}


}
