package net.ionoff.center.server.controller;

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
import net.ionoff.center.server.exception.ChangeEntityIdException;
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.persistence.service.IModeSensorService;
import net.ionoff.center.shared.dto.MessageDto;
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
	public ModeSensorDto insertOrUpdate(@PathVariable("modeSensorId") Long modeSensorId,
			@RequestBody ModeSensorDto modeSensorDto,
			HttpServletRequest request) {
		
		final User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		
		if (!modeSensorId.equals(modeSensorDto.getId())) {
			throw new ChangeEntityIdException(modeSensorDto.toString());
		}
		
		if (modeSensorDto.izNew()) {
			logger.info("User " + user.getName() + " insert mode sensor. Mode: " +
						modeSensorDto.getModeId() + ", Sensor: " + modeSensorDto.getSensorId()  + 
						", Enabled: " + modeSensorDto.getEnabled());
			
			return modeSensorService.insertDto(user, modeSensorDto);
		}
		else {
			logger.info("User " + user.getName() + " update mode sensor. Id: "
					+ modeSensorDto.getId() + ", Enabled: " + modeSensorDto.getEnabled());
			
			return modeSensorService.updateDto(user, modeSensorDto);
		}
	}

	@RequestMapping(value = "modesensors/{modeSensorId}",
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public MessageDto delete(@PathVariable("modeSensorId") Long modeSensorId, 
			HttpServletRequest request) throws DeleteEntityException {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		logger.info("User " + user.getName() + " deletes mode-sensor. ID: " + modeSensorId);
		modeSensorService.deleteDtoById(user, modeSensorId);
		return MessageDto.success(modeSensorId);
	}
	
	@RequestMapping(value = "modesensors",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<ModeSensorDto> findBySensorId(@RequestParam("sensorId") Long sensorId,
			HttpServletRequest request) {
		
		final User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		
		return modeSensorService.findDtosBySensorId(sensorId);
	}
}
