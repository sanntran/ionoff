package net.ionoff.center.server.restapi;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.ChangeEntityIdException;
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.persistence.service.IControllerService;
import net.ionoff.center.shared.dto.ControllerDto;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;

@RestController
@EnableWebMvc
public class ControllerServiceController {

	private final Logger logger = Logger.getLogger(ControllerServiceController.class.getName());

	@Autowired
	private IControllerService controllerService;
	
	@RequestMapping(value = "controllers/{controllerId}", 
			method = RequestMethod.PUT, 
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public ControllerDto insertOrUpdate(@PathVariable("controllerId") Long controllerId, 
			@RequestBody ControllerDto controllerDto,
			HttpServletRequest request) throws UpdateEntityException {
		
		if (!controllerId.equals(controllerDto.getId()) && !controllerDto.isNew()) {
			throw new ChangeEntityIdException(controllerDto.toString());
		}
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkProjectPermission(user, controllerDto.getProjectId());
		if (controllerDto.isNew()) {
			logger.info("User " + user.getName() + " inserts controller: " + controllerDto.toString());
			return controllerService.insertDto(user, controllerDto);
		} else {
			logger.info("User " + user.getName() + " updates controller. " + controllerDto.toString());
			return  controllerService.updateDto(user, controllerDto);
		}
	}

	@RequestMapping(value = "controllers/{controllerId}", method = RequestMethod.DELETE, produces = "application/json; charset=utf-8")
	@ResponseBody
	public MessageDto delete(@PathVariable("controllerId") Long controllerId, HttpServletRequest request)
			throws DeleteEntityException {

		User user = RequestContextHolder.getUser();
		ControllerDto controllerDto = controllerService.requireDtoById(controllerId);
		RequestContextHolder.checkProjectPermission(user, controllerDto.getProjectId());
		logger.info("User " + user.getName() + " delete controller. Controller: " + controllerDto.toString());
		controllerService.deleteDtoById(user, controllerId);
		return MessageDto.success(controllerId);
	}

	@RequestMapping(value = "controllers", 
			method = RequestMethod.GET, 
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<ControllerDto> findByProjectId(@RequestParam("projectId") Long projectId) {
		RequestContextHolder.checkProjectPermission(RequestContextHolder.getUser(), projectId);
		return controllerService.findDtoByProjectId(projectId);
	}

	@RequestMapping(value = "controllers/count",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public Long countByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		RequestContextHolder.checkProjectPermission(RequestContextHolder.getUser(), criteriaDto.getProjectId());
		return controllerService.countByCriteria(criteriaDto);
	}
	
	@RequestMapping(value = "controllers/search",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<ControllerDto> searchByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		RequestContextHolder.checkProjectPermission(RequestContextHolder.getUser(), criteriaDto.getProjectId());
		List<ControllerDto> controllers = controllerService.searchByCriteria(criteriaDto);
		return controllers;
	}

}
