package net.ionoff.center.server.restapi;

import java.text.SimpleDateFormat;

import javax.servlet.http.HttpServletRequest;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate4.LocalSessionFactoryBean;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import net.ionoff.center.server.persistence.service.IVersionService;
import net.ionoff.center.server.thread.ServerThreadPool;
import net.ionoff.center.server.webhook.WebhookRequest;
import net.ionoff.center.server.webhook.WebhookResponse;

@RestController
@EnableWebMvc
public class WebhookServiceController {
	
	private final Logger logger = Logger.getLogger(WebhookServiceController.class.getName());

	private static final SimpleDateFormat TIME_FORMATTER = new SimpleDateFormat("HH:mm");
	private static final SimpleDateFormat DATE_FORMATTER = new SimpleDateFormat("dd/MM/yyyy");

	@Autowired
	private IVersionService versionService;
	
	@Autowired
	private LocalSessionFactoryBean sessionFactory;
	
	@Autowired
	private ServerThreadPool serverThreadPool;
	
	@RequestMapping(value = "webhook", 
			method = RequestMethod.POST, 
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public WebhookResponse getServerInfo(@RequestBody WebhookRequest webhookRequest, HttpServletRequest request) {
		 WebhookResponse resp = new WebhookResponse();
		 resp.setFulfillmentText("Hello San");
		 return resp;
	}
}
