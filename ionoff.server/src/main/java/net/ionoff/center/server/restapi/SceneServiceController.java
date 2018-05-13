package net.ionoff.center.server.restapi;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.QueryParam;

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

import net.ionoff.center.server.control.IControlService;
import net.ionoff.center.server.control.UnknownRelayDriverModelException;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.ChangeEntityIdException;
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.persistence.service.IDashboardService;
import net.ionoff.center.server.persistence.service.ISceneService;
import net.ionoff.center.server.relaydriver.api.RelayDriverException;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.SceneDto;

@RestController
@EnableWebMvc
public class SceneServiceController {

	private final Logger logger = Logger.getLogger(SceneServiceController.class.getName());

	@Autowired
	private ISceneService sceneService;
	@Autowired
	private IControlService controlService;
	@Autowired
	private IDashboardService dashboardService;
	

	@RequestMapping(value = "scenes/{sceneId}",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public SceneDto findById(@PathVariable("sceneId") Long sceneId, HttpServletRequest request) {
		
		User user = RequestContextHolder.getUser();
		SceneDto sceneDto = sceneService.finDtoById(sceneId);
		RequestContextHolder.checkZonePermission(user, sceneDto.getZoneId());
		return sceneDto;
	}
	
	@RequestMapping(value = "scenes/{sceneId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public SceneDto save(@PathVariable("sceneId") Long sceneId,
			@RequestBody SceneDto sceneDto, HttpServletRequest request) {
		
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkZonePermission(user, sceneDto.getZoneId());
		if (!sceneId.equals(sceneDto.getId()) && !sceneDto.izNew()) {
			throw new ChangeEntityIdException(sceneDto.toString());
		}
		if (sceneDto.izNew()) {
			logger.info("User " + user.getName() + " inserts scene. " + sceneDto.toString());
			return sceneService.insertDto(user, sceneDto);
		}
		else {
			logger.info("User " +  user.getName() + " updates scene. " + sceneDto.toString());
			return sceneService.updateDto(user, sceneDto);
		}
	}

	@RequestMapping(value = "scenes/{sceneId}",
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public MessageDto delete(@PathVariable("sceneId") Long sceneId,
			HttpServletRequest request) throws DeleteEntityException {
		User user = RequestContextHolder.getUser();
		logger.info("User " + user.getName() + " delete scene. ID: " + sceneId);
		sceneService.deleteDtoById(user, sceneId);
		return MessageDto.success(sceneId);
	}
	
	@RequestMapping(value = "scenes",
			params = {"projectId"},
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<SceneDto> findByProject(
			@QueryParam("projectId") Long projectId,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkProjectPermission(user, projectId);
		List<SceneDto> sceneDtos = sceneService.findDtoByUserProject(user, projectId);
		return sceneDtos;
	}
	
	@RequestMapping(value = "scenes",
			params = {"zoneId"},
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<SceneDto> findByZoneId(@QueryParam("zoneId") Long zoneId, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkZonePermission(user, zoneId);
		List<SceneDto> sceneDtos = sceneService.findDtoByUserZone(user, zoneId);
		return sceneDtos;
	}
	

	@RequestMapping(value = "scenes/count",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public Long countByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		RequestContextHolder.checkProjectPermission(RequestContextHolder.getUser(), criteriaDto.getProjectId());
		return sceneService.countByCriteria(criteriaDto);
	}
	
	@RequestMapping(value = "scenes/search",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<SceneDto> searchByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		RequestContextHolder.checkProjectPermission(RequestContextHolder.getUser(), criteriaDto.getProjectId());
		List<SceneDto> sceneDtos = sceneService.searchByCriteria(criteriaDto);
		return sceneDtos;
	}

	@RequestMapping(value = "scenes/{sceneId}/play",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public Map<String, Boolean> playById(@PathVariable("sceneId") Long sceneId, HttpServletRequest request) {
		controlService.playScene(sceneService.requireById(sceneId));
		// Key: #SceneID#DeviceID || #SceneID#DeviceID#RelayID
		// Value: boolean
		return new HashMap<String, Boolean>();
	}
	
	@RequestMapping(value = "scenes/{sceneId}/dashboards",
			params= {"zoneId"},
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public SceneDto addToZoneDashboard(@PathVariable("sceneId") Long sceneId, @RequestParam("zoneId") Long zoneId,
			HttpServletRequest request) throws RelayDriverException, UnknownRelayDriverModelException {
		User user = RequestContextHolder.getUser();
		SceneDto sceneDto = sceneService.requireDtoById(sceneId);
		if (zoneId == null || !zoneId.equals(sceneDto.getZoneId())) {
			throw new BadRequestException("Zone "  + zoneId + "  does not contain scene "  + sceneId);
		}
		RequestContextHolder.checkZonePermission(user, zoneId);
		dashboardService.addSceneToZoneDashboard(user, sceneId);
		return sceneDto;
	}
	
	@RequestMapping(value = "scenes/{sceneId}/dashboards",
			params= {"projectId"},
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public SceneDto addToProjectDashboard(@PathVariable("sceneId") Long sceneId, @RequestParam("projectId") Long projectId,
			HttpServletRequest request) throws RelayDriverException, UnknownRelayDriverModelException {
		User user = RequestContextHolder.getUser();
		SceneDto sceneDto = sceneService.requireDtoById(sceneId);
		if (projectId == null || !projectId.equals(sceneDto.getProjectId())) {
			throw new BadRequestException("Project " + projectId + " does not contain scene " + sceneId);
		}
		RequestContextHolder.checkZonePermission(user, sceneDto.getZoneId());
		dashboardService.addSceneToProjectDashboard(user, sceneId);
		return sceneDto;
	}
	
	@RequestMapping(value = "scenes/{sceneId}/dashboards",
			params= {"zoneId"},
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public SceneDto removeFromZoneDashboard(@PathVariable("sceneId") Long sceneId, @RequestParam("zoneId") Long zoneId,
			HttpServletRequest request) throws RelayDriverException, UnknownRelayDriverModelException {
		User user = RequestContextHolder.getUser();
		SceneDto sceneDto = sceneService.requireDtoById(sceneId);
		if (zoneId == null || !zoneId.equals(sceneDto.getZoneId())) {
			throw new BadRequestException("Zone "  + zoneId + "  does not contain scene "  + sceneId);
		}
		RequestContextHolder.checkZonePermission(user, zoneId);
		dashboardService.removeSceneFromZoneDashboard(user, sceneId);
		return sceneDto;
	}
	
	@RequestMapping(value = "scenes/{sceneId}/dashboards",
			params= {"projectId"},
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public SceneDto removeFromProjectDashboard(@PathVariable("sceneId") Long sceneId, @RequestParam("projectId") Long projectId,
			HttpServletRequest request) throws RelayDriverException, UnknownRelayDriverModelException {
		User user = RequestContextHolder.getUser();
		SceneDto sceneDto = sceneService.requireDtoById(sceneId);
		if (projectId == null || !projectId.equals(sceneDto.getProjectId())) {
			throw new BadRequestException("Project " + projectId + " does not contain scene " + sceneId);
		}
		RequestContextHolder.checkZonePermission(user, sceneDto.getZoneId());
		dashboardService.removeSceneFromProjectDashboard(user, sceneId);
		return sceneDto;
	}
}
