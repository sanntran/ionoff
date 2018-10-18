package net.ionoff.center.server.restcontroller;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.ws.rs.BadRequestException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import net.ionoff.center.server.service.IControlService;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.ChangeEntityIdException;
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.persistence.service.IDashboardService;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.controller.exception.ControllerRequestException;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.StatusDto;

@RestController
public class DeviceServiceController {

	private final Logger logger = LoggerFactory.getLogger(DeviceServiceController.class.getName());
	
	@Autowired
	private IDeviceService deviceService;
	
	@Autowired
	private IDashboardService dashboardService;
	
	@Autowired
	private IControlService controlService;

	@RequestMapping(value = "devices/search",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public List<DeviceDto> searchByCriteria(@RequestBody QueryCriteriaDto criteriaDto,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, criteriaDto.getProjectId());
		final List<DeviceDto> devices = deviceService.searchByCriteria(criteriaDto);
		return devices;
	}

	@RequestMapping(value = "devices/count",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public Long countByCriteria(@RequestBody QueryCriteriaDto criteriaDto,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, criteriaDto.getProjectId());
		return deviceService.countByCriteria(criteriaDto);
	}


	@RequestMapping(value = "devices/{deviceId}",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public DeviceDto findById(@PathVariable("deviceId") Long deviceId,
									HttpServletRequest request) throws UpdateEntityException {

		DeviceDto deviceDto = deviceService.requireDtoById(deviceId);
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkZonePermission(user, deviceDto.getZoneId());
		return deviceDto;
	}

	@RequestMapping(value = "devices/{deviceId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")

	public DeviceDto insertOrUpdate(@PathVariable("deviceId") Long deviceId,
			@RequestBody DeviceDto deviceDto, HttpServletRequest request) throws UpdateEntityException {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkZonePermission(user, deviceDto.getZoneId());
		if (!deviceId.equals(deviceDto.getId()) && !deviceDto.izNew()) {
			throw new ChangeEntityIdException(deviceDto.toString());
		}
		
		if (deviceDto.izNew()) {
			logger.info("User " + user.getName() + " inserts device: " + deviceDto.toString());
			return deviceService.insertDto(user, deviceDto);
		}
		else {
			logger.info("User " + user.getName() + " updates device: " + deviceDto.toString());
			return deviceService.updateDto(user, deviceDto);
		}
	}

	@RequestMapping(value = "devices/{deviceId}",
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")

	public MessageDto delete(@PathVariable("deviceId") Long deviceId,
			HttpServletRequest request) throws DeleteEntityException {

		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		DeviceDto deviceDto = deviceService.requireDtoById(deviceId);
		RequestContextHolder.checkZonePermission(user, deviceDto.getZoneId());
		logger.info("User " + user.getName() + " deletes device: " + deviceDto.toString());
		deviceService.deleteDtoById(user, deviceId);
		return MessageDto.success(deviceId);
	}

	@RequestMapping(value = "devices",
			params= {"zoneId"},
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public List<DeviceDto> findByZoneId(@RequestParam("zoneId") Long zoneId) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkZonePermission(user, zoneId);
		final List<DeviceDto> deviceDtos = deviceService.findDtoByUserZoneId(user, zoneId);
		return deviceDtos;
	}

	@RequestMapping(value = "devices",
			params= {"projectId"},
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public List<DeviceDto> findByProjectId(@RequestParam("projectId") Long projectId) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkProjectPermission(user, projectId);
		final List<DeviceDto> deviceDtos = deviceService.findDtoByUserProjectId(user, projectId);
		return deviceDtos;
	}

	@RequestMapping(value = "devices/{deviceId}/on",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public StatusDto turnOnDevice(@PathVariable("deviceId") Long deviceId,
			HttpServletRequest request) throws ControllerRequestException {
		User user = RequestContextHolder.getUser();
		DeviceDto deviceDto = deviceService.requireDtoById(deviceId);
		RequestContextHolder.checkZonePermission(user, deviceDto.getZoneId());
		return controlService.turnOnDevice(deviceService.requireById(deviceId));
	}

	@RequestMapping(value = "devices/{deviceId}/off",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public StatusDto turnOffDevice(@PathVariable("deviceId") Long deviceId,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		DeviceDto deviceDto = deviceService.requireDtoById(deviceId);
		RequestContextHolder.checkZonePermission(user, deviceDto.getZoneId());
		return controlService.turnOffDevice(deviceService.requireById(deviceId));
	}

	@RequestMapping(value = "devices/status",
			params = {"zoneId"},
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public List<StatusDto> getStatusByZoneId(@RequestParam("zoneId") Long zoneId, HttpServletRequest request) {
		final User user = RequestContextHolder.getUser();
		RequestContextHolder.checkZonePermission(user, zoneId);
		List<StatusDto> statusDtos = deviceService.getStatusByZoneId(user, zoneId);
		return statusDtos;
	}
	
	@RequestMapping(value = "devices/status",
			params = {"projectId"},
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public List<StatusDto> getStatusByProjectId(@RequestParam("projectId") Long projectId, HttpServletRequest request) {
		final User user = RequestContextHolder.getUser();
		RequestContextHolder.checkProjectPermission(user, projectId);
		List<StatusDto> statusDtos = deviceService.getStatusByProjectId(user, projectId);
		return statusDtos;
	}
	
	@RequestMapping(value = "devices/{deviceId}/dashboards",
			params= {"zoneId"},
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public DeviceDto addToZoneDashboard(@PathVariable("deviceId") Long deviceId, @RequestParam("zoneId") Long zoneId,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		DeviceDto deviceDto = deviceService.requireDtoById(deviceId);
		if (zoneId == null || !zoneId.equals(deviceDto.getZoneId())) {
			throw new BadRequestException("Zone "  + zoneId + "  does not contain device "  + deviceId);
		}
		RequestContextHolder.checkZonePermission(user, zoneId);
		dashboardService.addDeviceToZoneDashboard(user, deviceId);
		return deviceDto;
	}
	
	@RequestMapping(value = "devices/{deviceId}/dashboards",
			params= {"projectId"},
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public MessageDto addToProjectDashboard(@PathVariable("deviceId") Long deviceId, @RequestParam("projectId") Long projectId,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		DeviceDto deviceDto = deviceService.requireDtoById(deviceId);
		if (projectId == null || !projectId.equals(deviceDto.getProjectId())) {
			throw new BadRequestException("Project " + projectId + " does not contain device " + deviceId);
		}
		RequestContextHolder.checkProjectPermission(user, deviceDto.getProjectId());
		dashboardService.addDeviceToProjectDashboard(user, deviceId);
		return new MessageDto(HttpServletResponse.SC_OK, "OK");
	}
	
	@RequestMapping(value = "devices/{deviceId}/dashboards",
			params= {"zoneId"},
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")

	public MessageDto removeFromZoneDashboard(@PathVariable("deviceId") Long deviceId, @RequestParam("zoneId") Long zoneId,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		DeviceDto deviceDto = deviceService.requireDtoById(deviceId);
		if (zoneId == null || !zoneId.equals(deviceDto.getZoneId())) {
			throw new BadRequestException("Zone "  + zoneId + "  does not contain device "  + deviceId);
		}
		RequestContextHolder.checkZonePermission(user, zoneId);
		dashboardService.removeDeviceFromZoneDashboard(user, deviceId);
		return new MessageDto(HttpServletResponse.SC_OK, "OK");
	}
	
	@RequestMapping(value = "devices/{deviceId}/dashboards",
			params= {"projectId"},
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")

	public MessageDto removeFromProjectDashboard(@PathVariable("deviceId") Long deviceId, @RequestParam("projectId") Long projectId,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		DeviceDto deviceDto = deviceService.requireDtoById(deviceId);
		if (projectId == null || !projectId.equals(deviceDto.getProjectId())) {
			throw new BadRequestException("Project " + projectId + " does not contain device " + deviceId);
		}
		RequestContextHolder.checkZonePermission(user, deviceDto.getZoneId());
		dashboardService.removeDeviceFromProjectDashboard(user, deviceId);
		return new MessageDto(HttpServletResponse.SC_OK, "OK");
	}
	
}
