package net.ionoff.center.server.controller;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.QueryParam;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.persistence.service.ISceneDeviceService;
import net.ionoff.center.shared.dto.SceneDeviceDto;

@RestController
public class SceneDeviceServiceController {
	
	@Autowired
	private ISceneDeviceService sceneDeviceService;
	
	@RequestMapping(value = "scenedevices",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public List<SceneDeviceDto> findBySceneId(
			@QueryParam("sceneId") Long sceneId,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		List<SceneDeviceDto> sceneDeviceDtos = sceneDeviceService.findDtoBySceneId(sceneId);
		return sceneDeviceDtos;
	}
	
	@RequestMapping(value = "scenedevices/{sceneDeviceId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")

	public SceneDeviceDto save(@PathVariable("sceneDeviceId") Long sceneDeviceId,
			@RequestBody SceneDeviceDto sceneDeviceDto, HttpServletRequest request) {
		
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		if (!sceneDeviceId.equals(sceneDeviceDto.getId())) {
			throw new BadRequestException("Id is not match");
		}
		return sceneDeviceService.updateDto(user, sceneDeviceDto);
	}
}
