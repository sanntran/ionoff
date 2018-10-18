package net.ionoff.center.server.restcontroller;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
import net.ionoff.center.server.license.LicenseManager;
import net.ionoff.center.server.persistence.service.IProjectService;
import net.ionoff.center.server.persistence.service.IUserDeviceService;
import net.ionoff.center.server.persistence.service.IUserProjectService;
import net.ionoff.center.server.persistence.service.IUserSceneService;
import net.ionoff.center.server.persistence.service.IUserService;
import net.ionoff.center.server.persistence.service.IUserZoneService;
import net.ionoff.center.server.persistence.service.IZoneService;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.ProjectDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.UserDeviceDto;
import net.ionoff.center.shared.dto.UserDto;
import net.ionoff.center.shared.dto.UserProjectDto;
import net.ionoff.center.shared.dto.UserSceneDto;
import net.ionoff.center.shared.dto.UserZoneDto;
import net.ionoff.center.shared.dto.ZoneDto;

@RestController
public class UserServiceController {

	private final Logger logger = LoggerFactory.getLogger(UserServiceController.class.getName());
	
	@Autowired
	private IUserProjectService userProjectService;

	@Autowired
	private IUserService userService;
	
	@Autowired
	private IZoneService zoneService;

	@Autowired
	private IUserZoneService userZoneService;
	
	@Autowired
	private IUserDeviceService userDeviceService;

	@Autowired
	private IUserSceneService userSceneService;
	
	@Autowired
	private IProjectService projectService;
	
	@RequestMapping(value = "users/count",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public Long countByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		RequestContextHolder.checkProjectPermission(RequestContextHolder.getUser(), criteriaDto.getProjectId());
		return userService.countByCriteria(criteriaDto);
	}
	
	@RequestMapping(value = "users/search",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public List<UserDto> searchByCriteria(@RequestBody QueryCriteriaDto criteriaDto, HttpServletRequest request) {
		RequestContextHolder.checkProjectPermission(RequestContextHolder.getUser(), criteriaDto.getProjectId());
		List<UserDto> userDtos = userService.searchByCriteria(criteriaDto);
		return userDtos;
	}
	
	@RequestMapping(value = "projects/{projectId}/users/{userId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")

	public UserDto insertOrUpdate(@PathVariable("projectId") Long projectId, 
			@PathVariable("userId") Long userId,
			@RequestBody UserDto userDto, HttpServletRequest request) throws Exception {

		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);

		if (!userId.equals(userDto.getId()) && !userDto.izNew()) {
			throw new ChangeEntityIdException(userDto.toString());
		}
		
		if (userDto.izNew()) {
			logger.info("User " + user.getName() + " inserts user: " + userDto.toString());
			return userService.insertDto(user, userDto, projectId);
		}
		else {
			logger.info("User " + user.getName() + " updates user: " + userDto.toString());
			return userService.updateDto(user, userDto);
		}
	}

	@RequestMapping(value = "users/{userId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")

	public UserDto insertOrUpdate(@PathVariable("userId") Long userId,
			@RequestBody UserDto userDto, HttpServletRequest request) throws Exception {

		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);

		if (!userId.equals(userDto.getId()) && !userDto.izNew()) {
			throw new ChangeEntityIdException(userDto.toString());
		}
		
		if (userDto.izNew()) {
			logger.info("User " + user.getName() + " inserts user: " + userDto.toString());
			return userService.insertDto(user, userDto);
		}
		else {
			logger.info("User " + user.getName() + " updates user: " + userDto.toString());
			return userService.updateDto(user, userDto);
		}
	}

	@RequestMapping(value = "users/{userId}",
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")

	public MessageDto delete(@PathVariable("userId") Long userId,
			HttpServletRequest request) {

		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		
		logger.info("User " + user.getName() + " delete user. ID: " + userId);
		userService.deleteDtoById(user, userId);
		return MessageDto.success(userId);
	}
	
	@RequestMapping(value = "projects/{projectId}/users/{userId}",
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")

	public MessageDto delete(@PathVariable("projectId") Long projectId,
			@PathVariable("userId") Long userId,
			HttpServletRequest request) {

		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		
		logger.info("User " + user.getName() + " delete user. ID: " + userId);
		userService.deleteDtoById(user, userId, projectId);
		return MessageDto.success(userId);
	}

	@RequestMapping(value = "projects/{projectId}/users",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public List<UserDto> findByProjectId(@PathVariable("projectId") Long projectId) {
		RequestContextHolder.checkProjectPermission(RequestContextHolder.getUser(), projectId);
		final List<UserDto> userDtos = userService.findDtoByProjectId(projectId);
		return userDtos;
	}
	
	@RequestMapping(
			value = "users/license",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public MessageDto activate(@RequestParam("licenseKey") String licenseKey) throws IOException {
		if (LicenseManager.isValidKey(licenseKey)) {
			logger.info("Activate successfully. License key: " + licenseKey);
			List<String> licenseKeys = new ArrayList<>();
			licenseKeys.add(licenseKey);
			LicenseManager.writeLicenseFiles(licenseKeys);
			LicenseManager.checkLicense();
			return MessageDto.success(licenseKey);
		}
		else {
			logger.info("Activate unsuccessfully. License key: " + licenseKey);
			return MessageDto.error(licenseKey);
		}
	}
	
	@RequestMapping(value = "userprojects/{userProjectId}",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public UserProjectDto updateUserProject(@PathVariable("userProjectId") Long userProjectId,
			@RequestBody UserProjectDto userProjectDto, HttpServletRequest request) {

		User user = RequestContextHolder.getUser();
		if (!User.LORD.equals(user.getName())) {
			RequestContextHolder.checkAdminPermission(user);
		}
		if (!userProjectId.equals(userProjectDto.getId()) && !userProjectDto.izNew()) {
			throw new ChangeEntityIdException(userProjectDto.toString());
		}
		logger.info("User " + user.getName() + " update user-project: " + userProjectDto.toString() + ", Role: " + userProjectDto.getRole());
		return userProjectService.updateDto(user, userProjectDto);
	}

	@RequestMapping(value = "users/{userId}/zones",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public List<ZoneDto> getZonesByUser(@PathVariable("userId") Long userId,
			@RequestParam("projectId") Long projectId) {
		
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkProjectPermission(user, projectId);
		
		return zoneService.findDtoByUserProjectId(userId, projectId);
	}
	
	@RequestMapping(value = "userzones/{userZoneId}",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public UserZoneDto updateUserZone(@PathVariable("userZoneId") Long userZoneId,
			@RequestBody UserZoneDto userZoneDto, HttpServletRequest request) {
		
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		
		logger.info("User " + user.getName()  + " update user-zone: "
				+ userZoneDto.toString() + ", Role: " + userZoneDto.getRole());
		
		return userZoneService.updateDto(user, userZoneDto);
	}
	
	@RequestMapping(value = "userdevices/{userDeviceId}",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public UserDeviceDto updateUserDevice(@PathVariable("userDeviceId") Long userDeviceId,
			@RequestBody UserDeviceDto userDeviceDto, HttpServletRequest request) {
		
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		
		logger.info("User " + user.getName()  + " update user-device: "
				+ userDeviceDto.toString() + ", Role: " + userDeviceDto.getRole());
		
		return userDeviceService.updateDto(user, userDeviceDto);
	}
	
	@RequestMapping(value = "userscenes/{userSceneId}",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public UserSceneDto updateUserDevice(@PathVariable("userSceneId") Long userSceneId,
			@RequestBody UserSceneDto userSceneDto, HttpServletRequest request) {
		
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		
		logger.info("User " + user.getName()  + " update user-scene: "
				+ userSceneDto.toString() + ", Role: " + userSceneDto.getRole());
		
		return userSceneService.updateDto(user, userSceneDto);
	}
	
	@RequestMapping(value = "userzones",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public List<UserZoneDto> getUserZonesByProject(
			@RequestParam("userId") Long userId,
			@RequestParam("projectId") Long projectId) {

		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		
		return userZoneService.findDtoByUserProject(userId, projectId);
	}
	
	@RequestMapping(value = "userprojects",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public List<UserProjectDto> getUserProjectsByUser(@RequestParam("userId") Long userId) {

		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		
		return userProjectService.findDtoByUserId(userId);
	}
	
	@RequestMapping(value = "users/{userId}/projects",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public List<ProjectDto> getProjectsByUser(@PathVariable("userId") Long userId) {
		return projectService.findDtoByUserId(userId);
	}
	
	@RequestMapping(value = "users/{userId}/language",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public UserDto updateUserLanguage(@PathVariable("userId") Long userId, @RequestParam("language") String language) {
		User user = RequestContextHolder.getUser();
		return userService.updateLanguage(user, language);
	}
}
