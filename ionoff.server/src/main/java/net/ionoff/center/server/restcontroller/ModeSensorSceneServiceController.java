package net.ionoff.center.server.restcontroller;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.persistence.service.IModeSensorSceneService;
import net.ionoff.center.shared.dto.ModeSensorSceneDto;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

@RestController
@Transactional
public class ModeSensorSceneServiceController {

	private static final Logger logger = LoggerFactory.getLogger(ModeSensorSceneServiceController.class.getName());

	@Autowired
	private IModeSensorSceneService modeSensorSceneService;

	@RequestMapping(value = "modesensorscenes/{modeSensorSceneId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")

	public ModeSensorSceneDto update(@PathVariable("modeSensorSceneId") Long modeSensorSceneId,
			@RequestBody ModeSensorSceneDto modeSensorSceneDto,
			HttpServletRequest request) {

		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		logger.info("User " + user.getName() + " update mode-sensor-scene. Id: "
				+ modeSensorSceneDto.getId() + " scene: " + modeSensorSceneDto.getSceneName()); 
		
		return modeSensorSceneService.updateDto(user, modeSensorSceneDto);
	}

	@RequestMapping(value = "modesensors/{modeSensorId}/modesensorscenes",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public List<ModeSensorSceneDto> findByModeSensorId(@PathVariable("modeSensorId") Long modeSensorId,
			HttpServletRequest request) {
		
		return modeSensorSceneService.findByModeSensorId(modeSensorId);
	}
}
