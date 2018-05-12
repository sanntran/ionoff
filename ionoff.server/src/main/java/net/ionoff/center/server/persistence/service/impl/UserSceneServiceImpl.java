package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.hibernate.Cache;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.entity.UserScene;
import net.ionoff.center.server.entity.UserZone;
import net.ionoff.center.server.objmapper.UserMapper;
import net.ionoff.center.server.persistence.dao.IUserSceneDao;
import net.ionoff.center.server.persistence.dao.IUserZoneDao;
import net.ionoff.center.server.persistence.service.IUserSceneService;
import net.ionoff.center.shared.dto.UserSceneDto;

@Transactional
public class UserSceneServiceImpl extends AbstractGenericService<UserScene, UserSceneDto> implements IUserSceneService {
	
	private IUserSceneDao userSceneDao;
	
	@Autowired
	private IUserZoneDao userZoneDao;
	
	@Autowired
	private UserMapper userMapper;
	
	public UserSceneServiceImpl(IUserSceneDao userSceneDao) {
		this.userSceneDao = userSceneDao;
	}

	@Override
	protected IUserSceneDao getDao() {
		return userSceneDao;
	}
	
	@Override
	public UserScene update(UserScene entity) {
		super.update(entity);
		Cache cache = getDao().getSessionFactory().getCache();
		if (cache != null) {
		    cache.evictAllRegions();
		}
		return entity;
	}

	@Override
	public List<UserScene> findByUserProject(Long userId, Long projectId) {
		return userSceneDao.findByProjectId(userId, projectId);
	}

	@Override
	public UserSceneDto requireDtoById(long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public UserSceneDto insertDto(User user, UserSceneDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public UserSceneDto updateDto(User user, UserSceneDto dto) {
		final UserScene userScene = requireById(dto.getId());
		userScene.setRole(dto.getRole());
		update(userScene);
		if (userScene.hasRole()) {
			UserZone userZone = userZoneDao.findByUserZone(
					user.getId(), userScene.getScene().getZone().getId());
			if (!userZone.hasRole()) {
				userZone.setRole(true);
				userZoneDao.update(userZone);
			}
		}
		return userMapper.createUserSceneDto(userScene);
	}

	@Override
	public void deleteDtoById(User user, long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<UserSceneDto> findDtoByUserProject(long userId, long projectId) {
		List<UserSceneDto> userSceneDtos = new ArrayList<>();
		for (UserScene userScene : findByUserProject(userId, projectId)) {
			userSceneDtos.add(userMapper.createUserSceneDto(userScene));
		}
		return userSceneDtos;
	}

	@Override
	protected List<UserSceneDto> createDtoList(List<UserScene> entities) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<UserScene> findByUserZone(Long userId, Long zoneId) {
		return userSceneDao.findByZoneId(userId, zoneId);
	}

	@Override
	public List<UserSceneDto> findDtoByUserZone(long userId, long zoneId) {
		List<UserSceneDto> userSceneDtos = new ArrayList<>();
		for (UserScene userScene : findByUserZone(userId, zoneId)) {
			userSceneDtos.add(userMapper.createUserSceneDto(userScene));
		}
		return userSceneDtos;
	}
}
