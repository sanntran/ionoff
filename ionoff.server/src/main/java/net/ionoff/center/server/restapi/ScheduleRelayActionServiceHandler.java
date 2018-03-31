package net.ionoff.center.server.restapi;

import javax.servlet.http.HttpServletRequest;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import net.ionoff.center.server.exception.ChangeEntityIdException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.persistence.service.IScheduleActionService;
import net.ionoff.center.shared.dto.ScheduleRelayActionDto;

@RestController
@EnableWebMvc
public class ScheduleRelayActionServiceHandler {

	private static final Logger logger = Logger.getLogger(ScheduleRelayActionServiceHandler.class.getName());

	@Autowired
	private IScheduleActionService scheduleActionService;

	@RequestMapping(value = "schedulerelayactions/{scheduleRelayActionId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public ScheduleRelayActionDto update(@PathVariable("scheduleRelayActionId") Long scheduleRelayActionId,
			@RequestBody ScheduleRelayActionDto scheduleRelayActionDto,
			HttpServletRequest request) throws UpdateEntityException {
		
		if (!scheduleRelayActionId.equals(scheduleRelayActionDto.getId()) && !scheduleRelayActionDto.isNew()) {
			throw new ChangeEntityIdException(scheduleRelayActionDto.toString());
		}
		
		final String userName = RequestContextHolder.getUserName();
		
		logger.info("User " + userName + " update schedule-relay-action. ID: " 
				+ scheduleRelayActionDto.getId() + ", Action: " + scheduleRelayActionDto.getAction());
		
		return scheduleActionService.updateScheduleRelayActionDto(scheduleRelayActionDto);
	}

}
