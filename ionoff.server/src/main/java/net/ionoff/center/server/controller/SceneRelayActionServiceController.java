package net.ionoff.center.server.controller;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.ChangeEntityIdException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.persistence.service.ISceneActionService;
import net.ionoff.center.shared.dto.SceneRelayActionDto;

@RestController
public class SceneRelayActionServiceController {

	private static final Logger logger = LoggerFactory.getLogger(SceneRelayActionServiceController.class.getName());

	@Autowired
	private ISceneActionService sceneActionService;

	@RequestMapping(value = "scenerelayactions/{sceneRelayActionId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")

	public SceneRelayActionDto update(@PathVariable("sceneRelayActionId") Long sceneRelayActionId,
			@RequestBody SceneRelayActionDto sceneRelayActionDto,
			HttpServletRequest request) throws UpdateEntityException {

		if (!sceneRelayActionId.equals(sceneRelayActionDto.getId()) && !sceneRelayActionDto.izNew()) {
			throw new ChangeEntityIdException(sceneRelayActionDto.toString());
		}
		
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		
		logger.info("User " + user.getName() + " update scene-relay-action. Id: "
				+ sceneRelayActionDto.getId() + " Action: " + sceneRelayActionDto.getAction());

		return sceneActionService.updateSceneRelayActionDto(sceneRelayActionDto);
	}
}
