package net.ionoff.center.server.restapi;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.persistence.service.ISensorService;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.SensorDataDto;

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

	@RequestMapping(value = "sensordata/sumbyday",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public List<SensorDataDto> sumDataByDay(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		DeviceDto deviceDto = deviceService.requireDtoById(criteriaDto.getDeviceId());
		RequestContextHolder.checkZonePermission(user, deviceDto.getZoneId());
		List<SensorDataDto> sensorDataDtos = sensorService.getSumDataByDay(criteriaDto);
		return sensorDataDtos;
	}

    @RequestMapping(value = "sensordata/loadbyday",
            method = RequestMethod.POST,
            produces = "application/json; charset=utf-8")
    @ResponseBody
    public List<SensorDataDto> loadDataByDay(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
        User user = RequestContextHolder.getUser();
        DeviceDto deviceDto = deviceService.requireDtoById(criteriaDto.getDeviceId());
        RequestContextHolder.checkZonePermission(user, deviceDto.getZoneId());
        List<SensorDataDto> sensorDataDtos = sensorService.loadDataByDay(criteriaDto);
        return sensorDataDtos;
    }
    
    @RequestMapping(value = "sensordata/export", 
			method = RequestMethod.POST, 
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public MessageDto exportReport(HttpServletRequest request, 
			@RequestBody QueryCriteriaDto criteriaDto, 
			@RequestParam("fileType") String fileType)
			throws Exception {
    	
    	User user = RequestContextHolder.getUser();
        DeviceDto deviceDto = deviceService.requireDtoById(criteriaDto.getDeviceId());
        RequestContextHolder.checkZonePermission(user, deviceDto.getZoneId());
    	
		String reportFilePath = sensorService.exportDataToReport(criteriaDto, fileType);
		
		return new MessageDto(HttpServletResponse.SC_OK, reportFilePath);
	}

}
