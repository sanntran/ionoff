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
import net.ionoff.center.server.persistence.service.IRelayDriverService;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.RelayDriverDto;

@RestController
@EnableWebMvc
public class RelayDriverServiceController {

	private final Logger logger = Logger.getLogger(RelayDriverServiceController.class.getName());

	@Autowired
	private IRelayDriverService relayDriverService;
	
	@RequestMapping(value = "relaydrivers/{relayDriverId}", 
			method = RequestMethod.PUT, 
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public RelayDriverDto insertOrUpdate(@PathVariable("relayDriverId") Long relayDriverId, 
			@RequestBody RelayDriverDto relayDriverDto,
			HttpServletRequest request) throws UpdateEntityException {
		
		if (!relayDriverId.equals(relayDriverDto.getId()) && !relayDriverDto.isNew()) {
			throw new ChangeEntityIdException(relayDriverDto.toString());
		}
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkProjectPermission(user, relayDriverDto.getProjectId());
		if (relayDriverDto.isNew()) {
			logger.info("User " + user.getName() + " inserts relayDriver: " + relayDriverDto.toString());
			return relayDriverService.insertDto(user, relayDriverDto);
		} else {
			logger.info("User " + user.getName() + " updates relayDriver. " + relayDriverDto.toString());
			return  relayDriverService.updateDto(user, relayDriverDto);
		}
	}

	@RequestMapping(value = "relaydrivers/{relayDriverId}", method = RequestMethod.DELETE, produces = "application/json; charset=utf-8")
	@ResponseBody
	public MessageDto delete(@PathVariable("relayDriverId") Long relayDriverId, HttpServletRequest request)
			throws DeleteEntityException {

		User user = RequestContextHolder.getUser();
		RelayDriverDto relayDriverDto = relayDriverService.requireDtoById(relayDriverId);
		RequestContextHolder.checkProjectPermission(user, relayDriverDto.getProjectId());
		logger.info("User " + user.getName() + " delete relayDriver. RelayDriver: " + relayDriverDto.toString());
		relayDriverService.deleteDtoById(user, relayDriverId);
		return MessageDto.success(relayDriverId);
	}

	@RequestMapping(value = "relayDrivers", 
			method = RequestMethod.GET, 
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<RelayDriverDto> findByProjectId(@RequestParam("projectId") Long projectId) {
		RequestContextHolder.checkProjectPermission(RequestContextHolder.getUser(), projectId);
		return relayDriverService.findDtoByProjectId(projectId);
	}

	@RequestMapping(value = "relaydrivers/count",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public Long countByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		RequestContextHolder.checkProjectPermission(RequestContextHolder.getUser(), criteriaDto.getProjectId());
		return relayDriverService.countByCriteria(criteriaDto);
	}
	
	@RequestMapping(value = "relaydrivers/search",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<RelayDriverDto> searchByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		RequestContextHolder.checkProjectPermission(RequestContextHolder.getUser(), criteriaDto.getProjectId());
		List<RelayDriverDto> relayDrivers = relayDriverService.searchByCriteria(criteriaDto);
		return relayDrivers;
	}

}
