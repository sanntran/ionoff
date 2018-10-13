package net.ionoff.center.server.controller;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.QueryParam;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import net.ionoff.center.server.service.IControlService;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.ChangeEntityIdException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.persistence.service.IRelayGroupService;
import net.ionoff.center.server.persistence.service.IRelayService;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.RelayGroupDto;
import net.ionoff.center.shared.dto.StatusDto;

@RestController
public class RelayServiceController {

	private static final Logger logger = LoggerFactory.getLogger(RelayServiceController.class.getName());

	@Autowired
	private IRelayService relayService;
	@Autowired
	private IRelayGroupService relayGroupService;
	@Autowired
	private IControlService controlService;

	@RequestMapping(value = "relays/count",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public Long countByCriteria(@RequestBody QueryCriteriaDto criretiaDto,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, criretiaDto.getProjectId());
		return relayService.countByCriteria(criretiaDto);
	}
	
	@RequestMapping(value = "relays/search",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public List<RelayDto> searchByCriteria(@RequestBody QueryCriteriaDto criretiaDto,
			HttpServletRequest request) {

		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, criretiaDto.getProjectId());
		return relayService.searchByCriteria(criretiaDto);
	}


	@RequestMapping(value = "relays/{relayId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")

	public RelayDto update(@PathVariable("relayId") Long relayId,
			@RequestBody RelayDto relayDto, HttpServletRequest request) throws UpdateEntityException {

		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		if (!relayId.equals(relayDto.getId()) && !relayDto.izNew()) {
			throw new ChangeEntityIdException(relayDto.toString());
		}
		
		logger.info("User " + user.getName() + " update relay: " + relayDto.toString());
		return relayService.updateDto(user, relayDto);
	}
	
	@RequestMapping(value = "relays/{relayId}/close",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public StatusDto closeRelay(@PathVariable("relayId") Long relayId,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		
		logger.info("User " + user.getName() + " close relay. ID: " + relayId);
		return controlService.switchOn(relayId);
		
	}
	
	@RequestMapping(value = "relays/{relayId}/open",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public StatusDto openRelay(@PathVariable("relayId") Long relayId,
			HttpServletRequest request) {
		
		User user = RequestContextHolder.getUser();
		
		logger.info("User " + user.getName() + " opem relay. ID: " + relayId);
		return controlService.switchOff(relayId);
	}
	
	@RequestMapping(value = "relays/{relayId}/groups",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public List<RelayGroupDto> getRelayGroups(@PathVariable("relayId") Long relayId, 
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		return relayGroupService.findByRelayId(relayId);
	}

	@RequestMapping(value = "relays/{relayId}/groups",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public RelayGroupDto createRelayGroup(@PathVariable("relayId") Long relayId, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		logger.info("User " + user.getName() + " create relay group. Relay id: " + relayId);
		return relayGroupService.createRelayGroup(user, relayId);
	}
	
	@RequestMapping(value = "relaygroups/{groupId}",
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")

	public MessageDto deleteRelayGroup(@PathVariable("groupId") Long groupId, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		logger.info("User " + user.getName() + " delete relay-group. Relay-group id: " + groupId);
		relayGroupService.deleteDtoById(user, groupId);
		return MessageDto.success(groupId);
	}
	
	@RequestMapping(value = "relaygroups/{groupId}/relays",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public RelayGroupDto addRelayToGroup(@PathVariable("groupId") Long groupId, 
			@RequestBody RelayDto relayDto, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		return relayGroupService.addRelayToGroup(groupId, relayDto);
	}
	
	@RequestMapping(value = "relaygroups/{groupId}/relays/{relayId}",
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")

	public RelayGroupDto removeRelayFromGroup(@PathVariable("groupId") Long groupId,
			@PathVariable("relayId") Long relayId, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		return relayGroupService.removeRelayFromGroup(groupId, relayId);
	}
	
	@RequestMapping(value = "relaygroups/{groupId}/relays/{relayId}/leader",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")

	public MessageDto updateRelayLeader(@PathVariable("groupId") Long groupId, @PathVariable("relayId") Long relayId,
			@QueryParam("isLeader") Boolean isLeader, HttpServletRequest request) throws UpdateEntityException {

		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		logger.info("User " + user.getName() + " update relay leader. Relay id: " + relayId + ", isLeader: " + isLeader);
		relayGroupService.updateRelayLeader(user, groupId, relayId, isLeader);
		return MessageDto.success("Update successful");
	}
	
}
