package net.ionoff.center.server.restapi;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.QueryParam;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import net.ionoff.center.server.control.IControlService;
import net.ionoff.center.server.control.UnknownRelayDriverModelException;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.ChangeEntityIdException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.persistence.service.IRelayGroupService;
import net.ionoff.center.server.persistence.service.IRelayService;
import net.ionoff.center.server.relaydriver.api.RelayDriverException;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.RelayGroupDto;
import net.ionoff.center.shared.dto.StatusDto;

@RestController
@EnableWebMvc
public class RelayServiceController {

	private static final Logger logger = Logger.getLogger(RelayServiceController.class.getName());

	@Autowired
	private IRelayService relayService;
	@Autowired
	private IRelayGroupService relayGroupService;
	@Autowired
	private IControlService controlService;

	@RequestMapping(value = "relays/count",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public Long countByCriteria(@RequestBody QueryCriteriaDto criretiaDto,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		if (!user.hasAdminRole()) {
			throw new AccessDeniedException("Access denied");
		}
		RequestContextHolder.checkProjectPermission(user, criretiaDto.getProjectId());
		return relayService.countByCriteria(criretiaDto);
	}
	
	@RequestMapping(value = "relays/search",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<RelayDto> searchByCriteria(@RequestBody QueryCriteriaDto criretiaDto,
			HttpServletRequest request) {

		User user = RequestContextHolder.getUser();
		if (!user.hasAdminRole()) {
			throw new AccessDeniedException("Access denied");
		}
		RequestContextHolder.checkProjectPermission(user, criretiaDto.getProjectId());
		return relayService.searchByCriteria(criretiaDto);
	}


	@RequestMapping(value = "relays/{relayId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public RelayDto update(@PathVariable("relayId") Long relayId,
			@RequestBody RelayDto relayDto, HttpServletRequest request) throws UpdateEntityException {

		User user = RequestContextHolder.getUser();
		if (!user.hasAdminRole()) {
			throw new AccessDeniedException("Access denied");
		}
		if (!relayId.equals(relayDto.getId()) && !relayDto.isNew()) {
			throw new ChangeEntityIdException(relayDto.toString());
		}
		
		logger.info("User " + user.getName() + " update relay: " + relayDto.toString());
		return relayService.updateDto(user, relayDto);
	}
	
	@RequestMapping(value = "relays/{relayId}/close",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public StatusDto closeRelay(@PathVariable("relayId") Long relayId,
			HttpServletRequest request) throws RelayDriverException, UnknownRelayDriverModelException {
		User user = RequestContextHolder.getUser();
		
		logger.info("User " + user.getName() + " close relay. ID: " + relayId);
		return controlService.switchOn(relayId);
		
	}
	
	@RequestMapping(value = "relays/{relayId}/open",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public StatusDto openRelay(@PathVariable("relayId") Long relayId,
			HttpServletRequest request) {
		
		User user = RequestContextHolder.getUser();
		
		logger.info("User " + user.getName() + " opem relay. ID: " + relayId);
		return controlService.switchOff(relayId);
	}
	
	@RequestMapping(value = "relays/{relayId}/closeopen",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public StatusDto closeOpenRelay(@PathVariable("relayId") Long relayId,
			HttpServletRequest request) {
		
		User user = RequestContextHolder.getUser();
		
		logger.info("User " + user.getName() + " close-open relay. ID: " + relayId);
		return controlService.switchOnOff(relayId);
	}
	
	@RequestMapping(value = "relays/{relayId}/group",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public RelayGroupDto getRelayGroup(@PathVariable("relayId") Long relayId,
			HttpServletRequest request) {
		
		User user = RequestContextHolder.getUser();
		if (!user.hasAdminRole()) {
			throw new AccessDeniedException("Access denied");
		}
		return relayGroupService.findByRelayId(relayId);
	}
	
	@RequestMapping(value = "relays/{relayId}/group",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public RelayGroupDto addRelayToGroup(@PathVariable("relayId") Long relayId, 
			@RequestBody RelayDto relayDto, HttpServletRequest request) {
		
		User user = RequestContextHolder.getUser();
		if (!user.hasAdminRole()) {
			throw new AccessDeniedException("Access denied");
		}
		return relayGroupService.addRelayToGroup(relayId, relayDto);
	}
	
	@RequestMapping(value = "relays/{relayId}/group",
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public RelayGroupDto removeRelayFromGroup(@PathVariable("relayId") Long relayId, 
			@QueryParam("relayIdToRemove") Long relayIdToRemove, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		if (!user.hasAdminRole()) {
			throw new AccessDeniedException("Access denied");
		}
		return relayGroupService.removeRelayFromGroup(relayId, relayIdToRemove);
	}
	
	@RequestMapping(value = "relays/{relayId}/leader",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public RelayDto update(@PathVariable("relayId") Long relayId,
			@QueryParam("isLeader") Boolean isLeader, HttpServletRequest request) throws UpdateEntityException {

		User user = RequestContextHolder.getUser();
		if (!user.hasAdminRole()) {
			throw new AccessDeniedException("Access denied");
		}
		logger.info("User " + user.getName() + " update relay leader. Relay id: " + relayId + ", isLeader: " + isLeader);
		return relayService.updateRelayLeader(user, relayId, isLeader);
	}
}
