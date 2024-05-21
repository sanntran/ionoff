package net.ionoff.center.server.restcontroller;

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
import org.springframework.web.bind.annotation.RestController;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.ChangeEntityIdException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.persistence.service.ISceneActionService;
import net.ionoff.center.shared.dto.ScenePlayerActionDto;

@RestController
@Transactional
public class ScenePlayerActionServiceController {

	private static final Logger logger = LoggerFactory.getLogger(ScenePlayerActionServiceController.class.getName());

	@Autowired
	private ISceneActionService sceneActionService;

	@RequestMapping(value = "sceneplayeractions/{scenePlayerActionId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")

	public ScenePlayerActionDto update(@PathVariable("scenePlayerActionId") Long scenePlayerActionId,
			@RequestBody ScenePlayerActionDto scenePlayerActionDto,
			HttpServletRequest request) throws UpdateEntityException {

		if (!scenePlayerActionId.equals(scenePlayerActionDto.getId()) && !scenePlayerActionDto.izNew()) {
			throw new ChangeEntityIdException(scenePlayerActionDto.toString());
		}
		
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);

		logger.info("User " + user.getName() + " update scene-player-action. Id: "
				+ scenePlayerActionDto.getId() + " Action: " + scenePlayerActionDto.getAction());

		return sceneActionService.updateScenePlayerActionDto(scenePlayerActionDto);
	}
}

