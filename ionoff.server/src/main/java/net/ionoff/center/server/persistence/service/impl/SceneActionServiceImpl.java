package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.SceneAction;
import net.ionoff.center.server.entity.SceneDevice;
import net.ionoff.center.server.entity.ScenePlayerAction;
import net.ionoff.center.server.entity.SceneRelayAction;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.persistence.mapper.SceneMapper;
import net.ionoff.center.server.persistence.dao.ISceneActionDao;
import net.ionoff.center.server.persistence.dao.ISceneDeviceDao;
import net.ionoff.center.server.persistence.service.ISceneActionService;
import net.ionoff.center.shared.dto.SceneActionDto;
import net.ionoff.center.shared.dto.ScenePlayerActionDto;
import net.ionoff.center.shared.dto.SceneRelayActionDto;

@Service
@Transactional
public class SceneActionServiceImpl extends AbstractGenericService<SceneAction, SceneActionDto> implements ISceneActionService {

	private ISceneActionDao sceneActionDao;
	
	@Autowired
	private SceneMapper sceneMapper;
	
	@Autowired
	private ISceneDeviceDao sceneDeviceDao;

	@Autowired
	public SceneActionServiceImpl(ISceneActionDao sceneActionDao) {
		this.sceneActionDao = sceneActionDao;
	}

	@Override
	protected ISceneActionDao getDao() {
		return sceneActionDao;
	}

	@Override
	public SceneActionDto requireDtoById(long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public SceneActionDto insertDto(User user, SceneActionDto dto) {
		throw new UnsupportedOperationException();
	}
	
	@Override
	public SceneActionDto updateDto(User user, SceneActionDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void deleteDtoById(User user, long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<SceneActionDto> findDtoBySceneIdDeviceId(long sceneId, long deviceId) {
		SceneDevice sceneDevice = sceneDeviceDao.findBySceneIdDeviceId(sceneId, deviceId);
		if (sceneDevice == null) {
			return new ArrayList<>();
		}
		return sceneMapper.createSceneActionDtoList(sceneDevice.getActions());
	}

	@Override
	public ScenePlayerActionDto updateScenePlayerActionDto(ScenePlayerActionDto scenePlayerActionDto) {
		final ScenePlayerAction sceneAction = (ScenePlayerAction) requireById(scenePlayerActionDto.getId());
		sceneAction.setAction(scenePlayerActionDto.getAction());
		sceneAction.setVolume(scenePlayerActionDto.getVolume());
		sceneAction.setAlbum(scenePlayerActionDto.getAlbum());
		sceneAction.setAlbumType(scenePlayerActionDto.getAlbumType());
		update(sceneAction);
		return (ScenePlayerActionDto) sceneMapper.createSceneActionDto(sceneAction);
	}

	@Override
	public SceneRelayActionDto updateSceneRelayActionDto(SceneRelayActionDto sceneRelayActionDto) {
		final SceneRelayAction sceneAction = (SceneRelayAction) requireById(sceneRelayActionDto.getId());
		sceneAction.setAction(sceneRelayActionDto.getAction());
		update(sceneAction);
		return (SceneRelayActionDto) sceneMapper.createSceneActionDto(sceneAction);
	}

	@Override
	protected List<SceneActionDto> createDtoList(List<SceneAction> entities) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<SceneActionDto> findDtoBySceneDeviceId(long sceneDeviceId) {
		SceneDevice sceneDevice = sceneDeviceDao.findById(sceneDeviceId);
		if (sceneDevice == null) {
			return new ArrayList<>();
		}
		return sceneMapper.createSceneActionDtoList(sceneDevice.getActions());
	}
}
