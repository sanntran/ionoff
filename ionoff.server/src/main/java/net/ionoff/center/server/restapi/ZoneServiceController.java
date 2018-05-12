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
import net.ionoff.center.server.persistence.service.IZoneService;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.ZoneDto;

@RestController
@EnableWebMvc
public class ZoneServiceController {

	private final Logger logger = Logger.getLogger(ZoneServiceController.class.getName());

	@Autowired
	private IZoneService zoneService;
	
	@RequestMapping(value = "zones/count",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public Long countByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		return zoneService.countByCriteria(criteriaDto);
	}
	
	@RequestMapping(value = "zones/search",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<ZoneDto> searchByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		List<ZoneDto> zoneDtos = zoneService.searchByCriteria(criteriaDto);
		return zoneDtos;
	}

	@RequestMapping(value = "zones/{zoneId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public ZoneDto save(@PathVariable("zoneId") Long zoneId,
			@RequestBody ZoneDto zoneDto, HttpServletRequest request) throws UpdateEntityException {
		
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		
		if (!zoneId.equals(zoneDto.getId()) && !zoneDto.izNew()) {
			throw new ChangeEntityIdException(zoneDto.toString());
		}
		
		if (zoneDto.izNew()) {
			logger.info("User " + user.getName() + " inserts zone: " + zoneDto.toString());
			return zoneService.insertDto(user, zoneDto);
		}
		else {
			logger.info("User " +  user.getName() + " updates zone: " + zoneDto.toString());
			return zoneService.updateDto(user, zoneDto);
		}
	}

	@RequestMapping(value = "zones/{zoneId}",
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public MessageDto delete(@PathVariable("zoneId") Long zoneId,
			HttpServletRequest request) throws DeleteEntityException {
		
		ZoneDto zoneDto = zoneService.requireDtoById(zoneId);
		
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		
		logger.info("User " + user.getName() + " delete zone: " + zoneDto.toString());
		zoneService.deleteDtoById(user, zoneId);
		return MessageDto.success(zoneId);
	}
	
	
	@RequestMapping(value = "zones/{zoneId}",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public ZoneDto findById(@PathVariable("zoneId") Long zoneId,
			HttpServletRequest request) throws DeleteEntityException {
		
		ZoneDto zoneDto = zoneService.requireDtoById(zoneId);
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkZonePermission(user, zoneId);
		
		return zoneDto;
	}
	
	@RequestMapping(value = "projects/{projectId}/zones",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<ZoneDto> findByUserProjectId(@PathVariable("projectId") Long projectId,
			@RequestParam(value = "includingDevice", required = false) Boolean includingDevice) {

		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkProjectPermission(user, projectId);
		return zoneService.findDtoByUserProjectId(user.getId(), projectId);
	}
	
}
