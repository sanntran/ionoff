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
import net.ionoff.center.server.persistence.service.IModeSensorSceneService;
import net.ionoff.center.shared.dto.ModeSensorSceneDto;

@RestController
@EnableWebMvc
public class ModeSensorSceneServiceController {

	private static final Logger logger = Logger.getLogger(ModeSensorSceneServiceController.class.getName());

	@Autowired
	private IModeSensorSceneService modeSensorSceneService;

	@RequestMapping(value = "modesensorscenes/{modeSensorSceneId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public ModeSensorSceneDto update(@PathVariable("modeSensorSceneId") Long modeSensorSceneId,
			@RequestBody ModeSensorSceneDto modeSensorSceneDto,
			HttpServletRequest request) {

		User user = RequestContextHolder.getUser();
		
		logger.info("User " + user.getName() + " update mode-sensor-scene. Id: "
				+ modeSensorSceneDto.getId() + " scene: " + modeSensorSceneDto.getSceneName()); 
		
		return modeSensorSceneService.updateDto(user, modeSensorSceneDto);
	}

	@RequestMapping(value = "modesensors/{modeSensorId}/modesensorscenes",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<ModeSensorSceneDto> findByModeSensorId(@PathVariable("modeSensorId") Long modeSensorId,
			@RequestParam("detected") boolean detected,
			HttpServletRequest request) {
		
		return modeSensorSceneService.findByModeSensorId(modeSensorId, detected);
	}
}
