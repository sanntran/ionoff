package net.ionoff.center.server.restapi;

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
import net.ionoff.center.server.exception.ChangeEntityIdException;
import net.ionoff.center.server.persistence.service.IModeSensorService;
import net.ionoff.center.shared.dto.ModeSensorDto;

@RestController
@EnableWebMvc
public class ModeSensorServiceController {

	private static final Logger logger = Logger.getLogger(ModeSensorServiceController.class.getName());

	@Autowired
	private IModeSensorService modeSensorService;

	@RequestMapping(value = "modesensors/{modeSensorId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public ModeSensorDto update(@PathVariable("modeSensorId") Long modeSensorId,
			@RequestBody ModeSensorDto modeSensorDto,
			HttpServletRequest request) {

		if (!modeSensorId.equals(modeSensorDto.getId()) && !modeSensorDto.isNew()) {
			throw new ChangeEntityIdException(modeSensorDto.toString());
		}
		
		final User user = RequestContextHolder.getUser();

		logger.info("User " + user.getName() + " update mode sensor. Id: "
				+ modeSensorDto.getId() + ", Enabled: " + modeSensorDto.getEnabled() 
				+ ", Time buffer: " + modeSensorDto.getTimeBuffer());
		
		return modeSensorService.updateDto(user, modeSensorDto);
	}
}
