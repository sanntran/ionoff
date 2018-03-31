package net.ionoff.center.server.restapi;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.QueryParam;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.AccessDeniedException;
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
@EnableWebMvc
public class SceneDeviceServiceController {
	
	@Autowired
	private ISceneDeviceService sceneDeviceService;
	
	@RequestMapping(value = "scenedevices",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<SceneDeviceDto> findBySceneId(
			@QueryParam("sceneId") Long sceneId,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		if (!user.hasAdminRole()) {
			throw new AccessDeniedException("Access denied");
		}
		List<SceneDeviceDto> sceneDeviceDtos = sceneDeviceService.findDtoBySceneId(sceneId);
		return sceneDeviceDtos;
	}
	
	@RequestMapping(value = "scenedevices/{sceneDeviceId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public SceneDeviceDto save(@PathVariable("sceneDeviceId") Long sceneDeviceId,
			@RequestBody SceneDeviceDto sceneDeviceDto, HttpServletRequest request) {
		
		User user = RequestContextHolder.getUser();
		if (!sceneDeviceId.equals(sceneDeviceDto.getId())) {
			throw new BadRequestException("Id is not match");
		}
		if (!user.hasAdminRole()) {
			throw new AccessDeniedException("Access denied");
		}
		return sceneDeviceService.updateDto(user, sceneDeviceDto);
	}
}
