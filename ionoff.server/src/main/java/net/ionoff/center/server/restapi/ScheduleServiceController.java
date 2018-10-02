package net.ionoff.center.server.restapi;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.QueryParam;

import org.apache.log4j.Logger;
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
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.persistence.service.IScheduleService;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.ScheduleDto;

@RestController
@EnableWebMvc
public class ScheduleServiceController {

	private static final Logger logger = Logger.getLogger(ScheduleServiceController.class.getName());

	@Autowired
	private IScheduleService scheduleService;

	@RequestMapping(value = "schedules/count",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public Long countByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, criteriaDto.getProjectId());
		return scheduleService.countByCriteria(criteriaDto);
	}
	
	@RequestMapping(value = "schedules/search",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<ScheduleDto> searchByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, criteriaDto.getProjectId());		
		List<ScheduleDto> scheduleDtos = scheduleService.searchByCriteria(criteriaDto);
		return scheduleDtos;
	}

	@RequestMapping(value = "schedules/{scheduleId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public ScheduleDto update(@PathVariable("scheduleId") Long scheduleId,
			@RequestBody ScheduleDto scheduleDto, HttpServletRequest request) throws UpdateEntityException {

		if (!scheduleId.equals(scheduleDto.getId()) && !scheduleDto.izNew()) {
			throw new ChangeEntityIdException(scheduleDto.toString());
		}
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		
		if (scheduleDto.izNew()) {
			logger.info("User " + user.getName() + " inserts scheduler: " + scheduleDto.toString());
			return scheduleService.insertDto(user, scheduleDto);
		}
		else {
			logger.info("User " + user.getName() + " updates scheduler: " + scheduleDto.toString());
			return scheduleService.updateDto(user, scheduleDto);
		}
	}

	@RequestMapping(value = "schedules/{scheduleId}",
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public MessageDto delete(@PathVariable("scheduleId") Long scheduleId,
			HttpServletRequest request) throws DeleteEntityException {
		
		User user = RequestContextHolder.getUser();
		ScheduleDto scheduleDto = scheduleService.requireDtoById(scheduleId);
		RequestContextHolder.checkAdminPermission(user);
		
		logger.info("User " + user.getName() + " delete scheduler: " + scheduleDto.toString());
		scheduleService.deleteDtoById(user, scheduleId);
		return MessageDto.success(scheduleId);
	}
	
	@RequestMapping(value = "schedules",
			params={"zoneId"},
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<ScheduleDto> findByZoneId(@QueryParam("zoneId") Long zoneId) {
		final List<ScheduleDto> scheduleDtos = scheduleService.findDtoByZoneId(zoneId);
		return scheduleDtos;
	}

	@RequestMapping(value = "schedules",
			params={"projectId"},
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<ScheduleDto> findByProjectId(@QueryParam("projectId") Long projectId) {
		final List<ScheduleDto> scheduleDtos = scheduleService.findDtoByProjectId(projectId);
		return scheduleDtos;
	}
}
