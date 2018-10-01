package net.ionoff.webhook.controller;

import net.ionoff.webhook.dto.CenterDto;
import net.ionoff.webhook.dto.WebhookRequest;
import net.ionoff.webhook.dto.WebhookResponse;
import net.ionoff.webhook.model.Center;
import net.ionoff.webhook.repository.CenterRepository;
import net.ionoff.webhook.service.CenterService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.text.SimpleDateFormat;

@RestController
@RequestMapping(value = "/centers")
public class CenterController {
	
	private static final Logger logger = LoggerFactory.getLogger(CenterController.class);


	@Autowired
	CenterService centerService;

	@RequestMapping(value = "/{id}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public CenterDto update(@PathVariable(value = "id") String id,
										 @Valid CenterDto dto, HttpServletRequest request) {
		String ip = request.getRemoteAddr();
		dto.setIp(ip);
		return centerService.save(id, dto);
	}

}
