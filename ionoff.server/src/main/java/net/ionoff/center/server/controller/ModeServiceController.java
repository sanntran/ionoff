package net.ionoff.center.server.controller;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import net.ionoff.center.server.service.IControlService;
import net.ionoff.center.server.entity.Mode;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.ChangeEntityIdException;
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.persistence.service.IModeService;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.ModeDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;

@RestController
public class ModeServiceController {

	private final Logger logger = LoggerFactory.getLogger(ModeServiceController.class.getName());
	
	@Autowired
	private IModeService modeService;
	
	@Autowired
	private IControlService controlService;

	@RequestMapping(value = "modes/count",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public Long countByProject(@RequestBody QueryCriteriaDto criteria,
			HttpServletRequest request) {

		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, criteria.getProjectId());
		
		return modeService.countByCriteria(criteria);
	}
	
	@RequestMapping(value = "modes/search",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public List<ModeDto> searchByCriteria(@RequestBody QueryCriteriaDto criteria,
			HttpServletRequest request) {
		
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, criteria.getProjectId());
		final List<ModeDto> modeDtos = modeService.searchByCriteria(criteria);
		return modeDtos;
	}

	@RequestMapping(value = "modes/{modeId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")

	public ModeDto update(@PathVariable("modeId") Long modeId,
			@RequestBody ModeDto modeDto, HttpServletRequest request) {
		
		if (!modeId.equals(modeDto.getId()) && !modeDto.izNew()) {
			throw new ChangeEntityIdException(modeDto.toString());
		}
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, modeDto.getProjectId());

		if (modeDto.izNew()) {
			logger.info("User " + user.getName() + " inserts mode: " + modeDto.toString());
			return modeService.insertDto(user, modeDto);
		}
		else {
			logger.info("User " + user.getName() + " updates mode: " + modeDto.toString());
			return modeService.updateDto(user, modeDto);
		}
	}

	@RequestMapping(value = "modes/{modeId}",
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")

	public MessageDto delete(@PathVariable("modeId") Long modeId,
			HttpServletRequest request) throws DeleteEntityException {

		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		ModeDto modeDto = modeService.requireDtoById(modeId);
		RequestContextHolder.checkProjectPermission(user, modeDto.getProjectId());

		logger.info("User " + user.getName() + " delete mode: " + modeDto.toString());
		modeService.deleteDtoById(user, modeId);
		return MessageDto.success(modeId);
	}

	@RequestMapping(value = "modes",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public List<ModeDto> findByProjectId(@RequestParam("projectId") Long projectId,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkProjectPermission(user, projectId);
		final List<ModeDto> modeDtos = modeService.findDtoByProjectId(projectId);
		return modeDtos;
	}

	@RequestMapping(value = "modes/activated",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public ModeDto getActivatedMode(@RequestParam("projectId") Long projectId,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, projectId);
		return modeService.findActivatedDtoByProjectId(projectId);
	}
	
	@RequestMapping(value = "modes/{modeId}/activate",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public MessageDto activateMode(@PathVariable("modeId") Long modeId,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		Mode mode = modeService.findById(modeId);
		controlService.activateMode(mode);
		return MessageDto.success(true);
	}
}
