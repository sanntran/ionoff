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
import net.ionoff.center.server.persistence.service.IModeSensorUserService;
import net.ionoff.center.shared.dto.ModeSensorUserDto;

@RestController
@EnableWebMvc
public class ModeSensorUserServiceController {

	private static final Logger logger = Logger.getLogger(ModeSensorUserServiceController.class.getName());

	@Autowired
	private IModeSensorUserService modeSensorUserService;

	@RequestMapping(value = "modesensorusers/{modeSensorUserId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public ModeSensorUserDto update(@PathVariable("modeSensorUserId") Long modeSensorUserId,
			@RequestBody ModeSensorUserDto modeSensorUserDto,
			HttpServletRequest request) {
			
		final User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		logger.info("User " + user.getName() + " update mode-sensor-user. Id: "
				+ modeSensorUserDto.getId() + ". Send sms: "
				+ modeSensorUserDto.getSendSms() + ". Send email " + modeSensorUserDto.getSendEmail());

		return modeSensorUserService.updateDto(user, modeSensorUserDto);
	}

	@RequestMapping(value = "modesensors/{modeSensorId}/modesensorusers",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<ModeSensorUserDto> findByModeSensorId(@PathVariable("modeSensorId") Long modeSensorId,
			HttpServletRequest request) {

		return  modeSensorUserService.findByModeSensorId(modeSensorId);
	}
}
