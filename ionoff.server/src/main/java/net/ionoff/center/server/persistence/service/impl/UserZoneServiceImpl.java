package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.Scene;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.entity.UserDevice;
import net.ionoff.center.server.entity.UserScene;
import net.ionoff.center.server.entity.UserZone;
import net.ionoff.center.server.objmapper.UserMapper;
import net.ionoff.center.server.persistence.dao.IUserDeviceDao;
import net.ionoff.center.server.persistence.dao.IUserSceneDao;
import net.ionoff.center.server.persistence.dao.IUserZoneDao;
import net.ionoff.center.server.persistence.service.IUserZoneService;
import net.ionoff.center.shared.dto.UserDeviceDto;
import net.ionoff.center.shared.dto.UserSceneDto;
import net.ionoff.center.shared.dto.UserZoneDto;

@Transactional
public class UserZoneServiceImpl extends AbstractGenericService<UserZone, UserZoneDto> implements IUserZoneService {
	
	private IUserZoneDao userZoneDao;
	
	@Autowired
	private UserMapper userMapper;
	
	@Autowired
	private IUserDeviceDao userDeviceDao;

	@Autowired
	private IUserSceneDao userSceneDao;
	
	public UserZoneServiceImpl(IUserZoneDao userZoneDao) {
		this.userZoneDao = userZoneDao;
	}

	@Override
	protected IUserZoneDao getDao() {
		return userZoneDao;
	}
	
	@Override
	public UserZone insert(UserZone entity) {
		super.insert(entity);
		if (entity.getZone().getDevices() != null) {
			for (Device device : entity.getZone().getDevices()) {
				UserDevice userDevice = new UserDevice();
				userDevice.setUser(entity.getUser());
				userDevice.setDevice(device);
				userDevice.setProject(entity.getProject());
				userDevice.setRole(entity.hasRole());
				userDeviceDao.insert(userDevice);
			}
		}
		if (entity.getZone().getScenes() != null) {
			for (Scene scene : entity.getZone().getScenes()) {
				UserScene userScene = new UserScene();
				userScene.setUser(entity.getUser());
				userScene.setScene(scene);
				userScene.setProject(entity.getProject());
				userScene.setRole(entity.hasRole());
				userSceneDao.insert(userScene);
			}
		}
		return entity;
	}
	
	@Override
	public UserZone update(UserZone entity) {
		super.update(entity);
		userDeviceDao.updateRoleByUserZone(entity);
		return entity;
	}

	@Override
	public List<UserZone> findByUserProjectId(Long userId, Long projectId) {
		return userZoneDao.findByProjectId(userId, projectId);
	}

	@Override
	public UserZoneDto requireDtoById(long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public UserZoneDto insertDto(User user, UserZoneDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public UserZoneDto updateDto(User user, UserZoneDto dto) {
		final UserZone userZone = requireById(dto.getId());
		userZone.setRole(dto.getRole());
		update(userZone);
		
		return createUserZoneDto(userZone);
	}

	private UserZoneDto createUserZoneDto(final UserZone userZone) {
		
		List<UserDevice> userDevices 
					= userDeviceDao.findByUserZoneId(userZone.getUser().getId(), userZone.getZone().getId()); 
		
		UserZoneDto userZoneDto = userMapper.createUserZoneDto(userZone);
		
		List<UserDeviceDto> userDeviceDtos = new ArrayList<>();
		for (UserDevice userDevice : userDevices) {
			UserDeviceDto userDeviceDto = userMapper.createUserDeviceDto(userDevice);
			userDeviceDtos.add(userDeviceDto);
		}
		userZoneDto.setUserDevices(userDeviceDtos);
		
		List<UserScene> userScenes 
				= userSceneDao.findByZoneId(userZone.getUser().getId(), userZone.getZone().getId());
		List<UserSceneDto> userSceneDtos = new ArrayList<>();
		for (UserScene userScene : userScenes) {
			UserSceneDto userSceneDto = userMapper.createUserSceneDto(userScene);
			userSceneDtos.add(userSceneDto);
		}
		userZoneDto.setUserScenes(userSceneDtos);
		return userZoneDto;
	}

	@Override
	public void deleteDtoById(User user, long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<UserZoneDto> findDtoByUserProject(long userId, long projectId) {
		List<UserZoneDto> userZoneDtos = new ArrayList<>();
		for (UserZone userZone : findByUserProjectId(userId, projectId)) {
			userZoneDtos.add(createUserZoneDto(userZone));
		}
		return userZoneDtos;
	}

	@Override
	protected List<UserZoneDto> createDtoList(List<UserZone> entities) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void removeByUserProjectId(long userId, long projectId) {
		userZoneDao.removeByUserProjectId(userId, projectId);
		userDeviceDao.removeByUserProjectId(userId, projectId);
		userSceneDao.removeByUserProjectId(userId, projectId);
	}
}
