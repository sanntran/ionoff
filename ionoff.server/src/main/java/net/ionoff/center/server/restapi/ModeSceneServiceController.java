package net.ionoff.center.server.restapi;

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
import net.ionoff.center.server.persistence.service.IModeSceneService;
import net.ionoff.center.server.persistence.service.IModeService;
import net.ionoff.center.shared.dto.ModeDto;
import net.ionoff.center.shared.dto.ModeSceneDto;

@RestController
@EnableWebMvc
public class ModeSceneServiceController {

	private static final Logger logger = Logger.getLogger(ModeSceneServiceController.class.getName());

	@Autowired
	private IModeSceneService modeSceneService;
	@Autowired
	private IModeService modeService;

	@RequestMapping(value = "modescenes/{modeSceneId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public ModeSceneDto update(@PathVariable("modeSceneId") Long modeSceneId,
			@RequestBody ModeSceneDto modeSceneDto,
			HttpServletRequest request) {
		final User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkZonePermission(user, modeSceneDto.getZoneId());
		
		logger.info("User " + user.getName() + " update mode scene" + modeSceneDto.toString()); 
		return modeSceneService.updateDto(user, modeSceneDto);
	}
	
	@RequestMapping(value = "modes/{modeId}/modescenes",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<ModeSceneDto> findByModeId(@PathVariable("modeId") Long modeId,
			HttpServletRequest request) {
		
		final User user = RequestContextHolder.getUser();
		ModeDto modeDto = modeService.requireDtoById(modeId);
		RequestContextHolder.checkProjectPermission(user, modeDto.getProjectId());
		return modeSceneService.findByModeId(modeId);
	}
}
