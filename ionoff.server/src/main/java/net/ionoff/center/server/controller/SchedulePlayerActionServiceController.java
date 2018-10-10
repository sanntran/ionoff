package net.ionoff.center.server.controller;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.ChangeEntityIdException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.persistence.service.IScheduleActionService;
import net.ionoff.center.shared.dto.SchedulePlayerActionDto;

@RestController
public class SchedulePlayerActionServiceController {

	private static final Logger logger = LoggerFactory.getLogger(SchedulePlayerActionServiceController.class.getName());

	@Autowired
	private IScheduleActionService scheduleActionService;

	@RequestMapping(value = "scheduleplayeractions/{schedulePlayerActionId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")

	public SchedulePlayerActionDto update(@PathVariable("schedulePlayerActionId") Long schedulePlayerActionId,
			@RequestBody SchedulePlayerActionDto schedulePlayerActionDto,
			HttpServletRequest request) throws UpdateEntityException {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		if (!schedulePlayerActionId.equals(schedulePlayerActionDto.getId()) && !schedulePlayerActionDto.izNew()) {
			throw new ChangeEntityIdException(schedulePlayerActionDto.toString());
		}
		
		final String userName = RequestContextHolder.getUserName();
		logger.info("User " + userName + " update scheduler-mediaplayer-action. ID: "
				+ schedulePlayerActionDto.getId() + ", Action: " + schedulePlayerActionDto.getAction());
		
		return scheduleActionService.updateSchedulePlayerActionDto(schedulePlayerActionDto);
	}
}
