package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.entity.UserDevice;
import net.ionoff.center.server.entity.UserZone;
import net.ionoff.center.server.objmapper.UserMapper;
import net.ionoff.center.server.persistence.dao.IUserDeviceDao;
import net.ionoff.center.server.persistence.dao.IUserZoneDao;
import net.ionoff.center.server.persistence.service.IUserDeviceService;
import net.ionoff.center.shared.dto.UserDeviceDto;

@Transactional
public class UserDeviceServiceImpl extends AbstractGenericService<UserDevice, UserDeviceDto> implements IUserDeviceService {
	
	private IUserDeviceDao userDeviceDao;
	
	@Autowired
	private IUserZoneDao userZoneDao;
	
	@Autowired
	private UserMapper userMapper;
	
	public UserDeviceServiceImpl(IUserDeviceDao userDeviceDao) {
		this.userDeviceDao = userDeviceDao;
	}

	@Override
	protected IUserDeviceDao getDao() {
		return userDeviceDao;
	}

	@Override
	public List<UserDevice> findByUserProject(Long userId, Long projectId) {
		return userDeviceDao.findByUserProjectId(userId, projectId);
	}

	@Override
	public UserDeviceDto requireDtoById(long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public UserDeviceDto insertDto(User user, UserDeviceDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public UserDeviceDto updateDto(User user, UserDeviceDto dto) {
		final UserDevice userDevice = requireById(dto.getId());
		userDevice.setRole(dto.getRole());
		update(userDevice);
		if (userDevice.hasRole()) {
			UserZone userZone = userZoneDao.findByUserZone(
					user.getId(), userDevice.getDevice().getZone().getId());
			if (!userZone.hasRole()) {
				userZone.setRole(true);
				userZoneDao.update(userZone);
			}
		}
		return userMapper.createUserDeviceDto(userDevice);
	}

	@Override
	public void deleteDtoById(User user, long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<UserDeviceDto> findDtoByUserProject(long userId, long projectId) {
		List<UserDeviceDto> userDeviceDtos = new ArrayList<>();
		for (UserDevice userDevice : findByUserProject(userId, projectId)) {
			userDeviceDtos.add(userMapper.createUserDeviceDto(userDevice));
		}
		return userDeviceDtos;
	}

	@Override
	protected List<UserDeviceDto> createDtoList(List<UserDevice> entities) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<UserDevice> findByUserZone(Long userId, Long zoneId) {
		return userDeviceDao.findByUserZoneId(userId, zoneId);
	}

	@Override
	public List<UserDeviceDto> findDtoByUserZone(long userId, long zoneId) {
		List<UserDeviceDto> userDeviceDtos = new ArrayList<>();
		for (UserDevice userDevice : findByUserZone(userId, zoneId)) {
			userDeviceDtos.add(userMapper.createUserDeviceDto(userDevice));
		}
		return userDeviceDtos;
	}
}
