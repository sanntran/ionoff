package net.ionoff.center.server.restapi;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.ChangeEntityIdException;
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.persistence.service.ISensorService;
import net.ionoff.center.shared.dto.*;
import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

@RestController
@EnableWebMvc
public class SensorDataServiceController {

	private static final Logger logger = Logger.getLogger(SensorDataServiceController.class.getName());

	@Autowired
	private ISensorService sensorService;

	@Autowired
	private IDeviceService deviceService;

	@RequestMapping(value = "sensordata/count",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public Long countByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		DeviceDto deviceDto = deviceService.requireDtoById(criteriaDto.getDeviceId());
		RequestContextHolder.checkZonePermission(user, deviceDto.getZoneId());
		return sensorService.countDataByCriteria(criteriaDto);
	}
	
	@RequestMapping(value = "sensordata/search",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<SensorDataDto> searchByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		DeviceDto deviceDto = deviceService.requireDtoById(criteriaDto.getDeviceId());
		RequestContextHolder.checkZonePermission(user, deviceDto.getZoneId());
		List<SensorDataDto> sensorDataDtos = sensorService.searchDataByCriteria(criteriaDto);
		return sensorDataDtos;
	}

}
