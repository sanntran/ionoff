package net.ionoff.center.server.objmapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.password.PasswordEncoder;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.entity.UserDevice;
import net.ionoff.center.server.entity.UserProject;
import net.ionoff.center.server.entity.UserScene;
import net.ionoff.center.server.entity.UserZone;
import net.ionoff.center.server.persistence.service.IUserGroupService;
import net.ionoff.center.shared.dto.UserDeviceDto;
import net.ionoff.center.shared.dto.UserDto;
import net.ionoff.center.shared.dto.UserProjectDto;
import net.ionoff.center.shared.dto.UserSceneDto;
import net.ionoff.center.shared.dto.UserZoneDto;

public class UserMapper {


	@Autowired 
	private PasswordEncoder passwordEncoder;
	
	@Autowired 
	private IUserGroupService groupService;
	
	public List<UserDto> createUserDtoList(List<User> users) {
		final List<UserDto>  userDtos = new ArrayList<UserDto>();
		for (final User user : users) {
			userDtos.add(createUserDto(user));
		}
		return userDtos;
	}

	
	public User createUser(UserDto userDto) {
		final User user = new User();
		updateUser(user, userDto);
		return user;
	}
	
	public User updateUser(User user, UserDto userDto) {
		user.setName(userDto.getName());
		if (!UserDto.PASSWORD_DISPLAY.equals(userDto.getPassword())) {
			final String encryptedPass = passwordEncoder.encode(userDto.getPassword());
			user.setPassword(encryptedPass);
		}
		user.setFullName(userDto.getFullName());
		user.setPhoneNo(userDto.getPhoneNo());
		user.setEmail(userDto.getEmail());
		user.setGroup(groupService.findByName(userDto.getGroupName()));
		return user;
	}
	
	public UserDto createUserDto(User user) {
		final UserDto userDto = new UserDto();
		userDto.setId(user.getId());
		userDto.setName(user.getName());
		userDto.setFullName(user.getFullName());
		userDto.setPassword(UserDto.PASSWORD_DISPLAY);
		userDto.setPhoneNo(user.getPhoneNo());
		userDto.setEmail(user.getEmail());
		userDto.setGroupName(user.getGroup().getName());
		return userDto;
	}
	
	public UserProjectDto toUserProjectDto(UserProject userProject) {
		UserProjectDto userProjectDto = new UserProjectDto();
		userProjectDto.setId(userProject.getId());
		userProjectDto.setProjectId(userProject.getProject().getId());
		userProjectDto.setProjectName(userProject.getProject().getName());
		userProjectDto.setProjectDesc(userProject.getProject().getAddress());
		userProjectDto.setUserId(userProject.getUser().getId());
		userProjectDto.setUserName(userProject.getUser().getName());
		userProjectDto.setRole(userProject.getRole());
		return userProjectDto;
	}
	
	public UserZoneDto createUserZoneDto(UserZone userZone) {
		UserZoneDto userZoneDto = new UserZoneDto();
		userZoneDto.setId(userZone.getId());
		userZoneDto.setZoneId(userZone.getZone().getId());
		userZoneDto.setZoneName(userZone.getZone().getName());
		userZoneDto.setAreaName(userZone.getZone().getArea().getName());
		userZoneDto.setUserId(userZone.getUser().getId());
		userZoneDto.setUserName(userZone.getUser().getName());
		userZoneDto.setProjectId(userZone.getZone().getProject().getId());
		userZoneDto.setRole(userZone.getRole());
		return userZoneDto;
	}


	public UserDeviceDto createUserDeviceDto(UserDevice userDevice) {
		UserDeviceDto userDeviceDto = new UserDeviceDto();
		userDeviceDto.setId(userDevice.getId());
		userDeviceDto.setDeviceId(userDevice.getDevice().getId());
		userDeviceDto.setDeviceName(userDevice.getDevice().getName());
		userDeviceDto.setUserId(userDevice.getUser().getId());
		userDeviceDto.setUserName(userDevice.getUser().getName());
		userDeviceDto.setProjectId(userDevice.getDevice().getProject().getId());
		userDeviceDto.setRole(userDevice.getRole());
		return userDeviceDto;
	}


	public UserSceneDto createUserSceneDto(UserScene userScene) {
		UserSceneDto userSceneDto = new UserSceneDto();
		userSceneDto.setId(userScene.getId());
		userSceneDto.setSceneId(userScene.getScene().getId());
		userSceneDto.setSceneName(userScene.getScene().getName());
		userSceneDto.setUserId(userScene.getUser().getId());
		userSceneDto.setUserName(userScene.getUser().getName());
		userSceneDto.setProjectId(userScene.getScene().getZone().getProject().getId());
		userSceneDto.setRole(userScene.getRole());
		return userSceneDto;
	}
}
