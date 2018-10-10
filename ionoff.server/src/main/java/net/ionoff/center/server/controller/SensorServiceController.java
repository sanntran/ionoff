package net.ionoff.center.server.controller;

import java.util.List;

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

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.ChangeEntityIdException;
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.persistence.service.ISensorService;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.SensorDto;

@RestController
public class SensorServiceController {

	private static final Logger logger = Logger.getLogger(SensorServiceController.class.getName());

	@Autowired
	private ISensorService sensorService;

	@RequestMapping(value = "sensors/count",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public Long countByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkProjectPermission(user, criteriaDto.getProjectId());
		return sensorService.countByCriteria(criteriaDto);
	}
	
	@RequestMapping(value = "sensors/search",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public List<SensorDto> searchByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkProjectPermission(user, criteriaDto.getProjectId());
		List<SensorDto> sensorDtoDtos = sensorService.searchByCriteria(criteriaDto);
		return sensorDtoDtos;
	}

	@RequestMapping(value = "sensors/{sensorId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")

	public SensorDto update(@PathVariable("sensorId") Long sensorId,
			@RequestBody SensorDto sensorDto, HttpServletRequest request) throws UpdateEntityException {

		if (!sensorId.equals(sensorDto.getId()) && !sensorDto.izNew()) {
			throw new ChangeEntityIdException(sensorDto.toString());
		}
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, sensorDto.getProjectId());
		
		if (sensorDto.izNew()) {
			logger.info("User " + user.getName() + " inserts sensor: " + sensorDto.toString());
			return sensorService.insertDto(user, sensorDto);
		}
		else {
			logger.info("User " + user.getName() + " updates sensor: " + sensorDto.toString());
			return sensorService.updateDto(user, sensorDto);
		}
	}

	@RequestMapping(value = "sensors/{sensorId}",
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")

	public MessageDto delete(@PathVariable("sensorId") Long sensorId,
			HttpServletRequest request) throws DeleteEntityException {
	
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		SensorDto sensorDto = sensorService.requireDtoById(sensorId);
		RequestContextHolder.checkProjectPermission(user, sensorDto.getProjectId());
		
		logger.info("User " + user.getName() + " delete sensor: " + sensorDto.toString());
		sensorService.deleteDtoById(user, sensorId);
		return MessageDto.success(sensorId);
	}

	@RequestMapping(value = "sensors",
			params= {"projectId"},
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public List<SensorDto> findByProjectId(@PathVariable("projectId") Long projectId) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkProjectPermission(user, projectId);
		final List<SensorDto> sensorDtos = sensorService.findDtoByProjectId(projectId);
		return sensorDtos;
	}
}
