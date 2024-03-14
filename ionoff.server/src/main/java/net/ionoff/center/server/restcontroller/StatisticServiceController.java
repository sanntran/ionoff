package net.ionoff.center.server.restcontroller;

import net.ionoff.center.shared.dto.AlertStatisticDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class StatisticServiceController {

	private final Logger logger = LoggerFactory.getLogger(StatisticServiceController.class.getName());


	@RequestMapping(value = "statistics/alert",
			params= {"projectId"},
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public AlertStatisticDto getAlertStatisticByProjectId(@RequestParam("projectId") Long projectId) {
		AlertStatisticDto result = new AlertStatisticDto();
		result.setTotalCount(1);
		return result;
	}
	
}
