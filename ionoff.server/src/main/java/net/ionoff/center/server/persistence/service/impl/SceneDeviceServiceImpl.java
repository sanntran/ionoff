package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Player;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.Scene;
import net.ionoff.center.server.entity.SceneAction;
import net.ionoff.center.server.entity.SceneDevice;
import net.ionoff.center.server.entity.ScenePlayerAction;
import net.ionoff.center.server.entity.SceneRelayAction;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.objmapper.SceneMapper;
import net.ionoff.center.server.persistence.dao.ISceneActionDao;
import net.ionoff.center.server.persistence.dao.ISceneDao;
import net.ionoff.center.server.persistence.dao.ISceneDeviceDao;
import net.ionoff.center.server.persistence.service.ISceneDeviceService;
import net.ionoff.center.shared.dto.SceneActionDto;
import net.ionoff.center.shared.dto.SceneDeviceDto;
import net.ionoff.center.shared.dto.ScenePlayerActionDto;
import net.ionoff.center.shared.dto.SceneRelayActionDto;

@Transactional
public class SceneDeviceServiceImpl extends AbstractGenericService<SceneDevice, SceneDeviceDto> implements ISceneDeviceService {

	private ISceneDeviceDao sceneDeviceDao;
	
	@Autowired
	private ISceneActionDao sceneActionDao;
	
	@Autowired
	private ISceneDao sceneDao;
	
	@Autowired
	private SceneMapper sceneMapper;
	
	public SceneDeviceServiceImpl(ISceneDeviceDao sceneDeviceDao) {
		this.sceneDeviceDao = sceneDeviceDao;
	}

	@Override
	protected ISceneDeviceDao getDao() {
		return sceneDeviceDao;
	}
	
	@Override
	public SceneDevice insert(SceneDevice sceneDevice) {
		super.insert(sceneDevice);
		insertSceneActions(sceneDevice);
		return sceneDevice;
	}
	
	private void insertSceneActions(SceneDevice sceneDevice) {
		if (sceneDevice.getDevice() instanceof Player) {
			insertScenePlayerAction(sceneDevice);
		}
		else {
			insertSceneRelayActions(sceneDevice);
		}
	}

	private void insertSceneRelayActions(SceneDevice sceneDevice) {
		List<Relay> relays = sceneDevice.getDevice().getRelayList();
		for (Relay relay : relays) {
			SceneRelayAction sceneAction = new SceneRelayAction();
			sceneAction.setRelay(relay);
			sceneAction.setSceneDevice(sceneDevice);
			sceneAction.setAction(SceneRelayAction.NONE);
			sceneActionDao.insert(sceneAction);
		}
	}

	private void insertScenePlayerAction(SceneDevice sceneDevice) {
		ScenePlayerAction sceneAction = new ScenePlayerAction();
		sceneAction.setPlayer((Player)sceneDevice.getDevice());
		sceneAction.setAction(ScenePlayerAction.NONE);
		sceneAction.setSceneDevice(sceneDevice);
		sceneActionDao.insert(sceneAction);
	}

	@Override
	public SceneDevice findBySceneIdDeviceId(long sceneId, long deviceId) {
		return sceneDeviceDao.findBySceneIdDeviceId(sceneId, deviceId);
	}

	@Override
	public SceneDeviceDto requireDtoById(long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public SceneDeviceDto insertDto(User user, SceneDeviceDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public SceneDeviceDto updateDto(User user, SceneDeviceDto dto) {
		SceneDevice sceneDevice = requireById(dto.getId());
		sceneDevice.setOrder(dto.getOrder());
		sceneDevice.setDuration(dto.getDuration());
		if (dto.getActions() != null && !dto.getActions().isEmpty() 
				&& sceneDevice.getActions() != null && !sceneDevice.getActions().isEmpty()) {
			updateSceneActions(dto.getActions(), sceneDevice.getActions());
		}
		return sceneMapper.createSceneDeviceDto(sceneDevice);
	}

	private void updateSceneActions(List<SceneActionDto> sceneActionDtos, List<SceneAction> sceneActions) {
		for (SceneActionDto sceneActionDto : sceneActionDtos) {
			for (SceneAction sceneAction : sceneActions) {
				if (sceneAction.getId() == sceneAction.getId()) {
					updateSceneAction(sceneActionDto, sceneAction);
					break;
				}
			}
		}
	}

	private void updateSceneAction(SceneActionDto sceneActionDto, SceneAction sceneAction) {
		if (sceneActionDto instanceof SceneRelayActionDto) {
			SceneRelayActionDto sceneRelayActionDto = (SceneRelayActionDto) sceneActionDto;
			SceneRelayAction sceneRelayAction = (SceneRelayAction) sceneAction;
			sceneRelayAction.setAction(sceneRelayActionDto.getAction());
			sceneActionDao.update(sceneRelayAction);
		}
		else if (sceneActionDto instanceof ScenePlayerActionDto) {
			ScenePlayerActionDto scenePlayerActionDto = (ScenePlayerActionDto) sceneActionDto;
			ScenePlayerAction scenePlayerAction = (ScenePlayerAction) sceneAction;
			scenePlayerAction.setAction(scenePlayerActionDto.getAction());
			scenePlayerAction.setAlbum(scenePlayerActionDto.getAlbum());
			scenePlayerAction.setVolume(scenePlayerActionDto.getVolume());
			scenePlayerAction.setAlbumType(scenePlayerActionDto.getAlbumType());
			sceneActionDao.update(scenePlayerAction);
		}
	}

	@Override
	public void deleteDtoById(User user, long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public SceneDeviceDto findDtoBySceneIdDeviceId(long sceneId, long deviceId) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected List<SceneDeviceDto> createDtoList(List<SceneDevice> entities) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<SceneDeviceDto> findDtoBySceneId(Long sceneId) {
		Scene scene = sceneDao.findById(sceneId);
		if (scene.getDevices() == null || scene.getDevices().isEmpty()) {
			return Collections.emptyList();
		}
		List<SceneDeviceDto> sceneDeviceDtos = new ArrayList<>();
		for (SceneDevice sceneDevice : scene.getDevices()) {
			sceneDeviceDtos.add(sceneMapper.createSceneDeviceDto(sceneDevice));
		}
		return sceneDeviceDtos;
	}
}
