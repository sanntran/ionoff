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
import net.ionoff.center.server.persistence.service.IAreaService;
import net.ionoff.center.shared.dto.AreaDto;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;

@RestController
@EnableWebMvc
public class AreaServiceController {

	private final Logger logger = Logger.getLogger(AreaServiceController.class.getName());
	
	@Autowired
	private IAreaService areaService;

	@RequestMapping(value = "areas",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public AreaDto insert(@RequestBody AreaDto areaDto, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, areaDto.getProjectId());
		logger.info("User " + user.getName() + " inserts new area. " + areaDto.toString());
		return areaService.insertDto(user, areaDto);
	}
	 
	@RequestMapping(value = "areas/{areaId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public AreaDto insertOrUpdate(@PathVariable("areaId" ) Long areaId,
						  @RequestBody AreaDto areaDto, HttpServletRequest request) {
		if (!areaId.equals(areaDto.getId()) && !areaDto.izNew()) {
			throw new ChangeEntityIdException(areaDto.toString());
		}
		
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, areaDto.getProjectId());
		if (areaDto.izNew()) {
			logger.info("User " + user.getName() + " inserts new area. " + areaDto.toString());
			return areaService.insertDto(user, areaDto);
		}
		else {
			logger.info("User " +  user.getName() + " updates area. " + areaDto.toString());
			return areaService.updateDto(user, areaDto);
		}
	}

	@RequestMapping(value = "areas/{areaId}",
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public MessageDto delete(@PathVariable("areaId") Long areaId, 
			HttpServletRequest request) throws DeleteEntityException {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkAreaPermission(user, areaId);
		logger.info("User " + user.getName() + " deletes area. Area ID: " + areaId);
		areaService.deleteDtoById(user, areaId);
		return MessageDto.success(areaId);
	}
	
	@RequestMapping(value = "areas",
			method = RequestMethod.GET,
			params = {"projectId"},
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<AreaDto> findByProject(@RequestParam("projectId") Long projectId, 
			@RequestParam(value = "includingZone") Boolean includingZone,
			@RequestParam(value = "includingDevice") Boolean includingDevice,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkProjectPermission(user, projectId);
		final List<AreaDto> areas = areaService.findByProjectId(projectId, includingZone, includingDevice);
		return areas;
	}

	@RequestMapping(value = "areas/count",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public Long countByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, criteriaDto.getProjectId());
		return areaService.countByCriteria(criteriaDto);
	}
	
	@RequestMapping(value = "areas/search",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<AreaDto> searchByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, criteriaDto.getProjectId());
		List<AreaDto> areas = areaService.searchByCriteria(criteriaDto);
		return areas;
	}

}
