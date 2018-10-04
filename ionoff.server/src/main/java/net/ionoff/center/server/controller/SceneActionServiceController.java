package net.ionoff.center.server.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.persistence.service.ISceneActionService;
import net.ionoff.center.shared.dto.SceneActionDto;

@RestController
@EnableWebMvc
public class SceneActionServiceController {

	@Autowired
	private ISceneActionService sceneActionService;

	@RequestMapping(value = "sceneactions",
			params = {"sceneDeviceId"},
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<SceneActionDto> findBySceneDevice(
			@RequestParam("sceneDeviceId") long sceneDeviceId) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		List<SceneActionDto> sceneActionDtos = sceneActionService.findDtoBySceneDeviceId(sceneDeviceId);
		return sceneActionDtos;
	}

}