package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.Scene;
import net.ionoff.center.server.entity.SceneDevice;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.entity.UserScene;
import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.server.persistence.mapper.SceneMapper;
import net.ionoff.center.server.persistence.dao.ISceneDao;
import net.ionoff.center.server.persistence.dao.IUserDao;
import net.ionoff.center.server.persistence.dao.IUserSceneDao;
import net.ionoff.center.server.persistence.service.ISceneDeviceService;
import net.ionoff.center.server.persistence.service.ISceneService;
import net.ionoff.center.server.persistence.service.IZoneService;
import net.ionoff.center.shared.dto.SceneDto;

@Service
@Transactional
public class SceneServiceImpl extends AbstractGenericService<Scene, SceneDto> implements ISceneService {

	private ISceneDao sceneDao;
	
	@Autowired
	private IUserDao userDao;
	
	@Autowired
	private IZoneService zoneService;
	
	@Autowired
	private IUserSceneDao userSceneDao;
	
	@Autowired
	private ISceneDeviceService sceneDeviceService;
	
	@Autowired
	private SceneMapper sceneMapper;

	@Autowired
	public SceneServiceImpl(ISceneDao sceneDao) {
		this.sceneDao = sceneDao;
		
	}

	@Override
	protected ISceneDao getDao() {
		return sceneDao;
	}

	@Override
	public Scene insert(Scene scene) {
		super.insert(scene);
		insertSceneDevices(scene);
		insertUserScenes(scene);
		return scene;
	}

	private void insertUserScenes(Scene scene) {
		List<User> users = userDao.findByProjectId(scene.getZone().getProject().getId());
		for (User user : users) { 
			UserScene userScene = new UserScene();
			userScene.setUser(user);
			userScene.setScene(scene);
			if (user.hasAdminRole()) {
				userScene.setRole(true);
			}
			else {
				userScene.setRole(false);
			}
			userScene.setProject(scene.getZone().getProject());
			userSceneDao.insert(userScene);
		}
	}

	private void insertSceneDevices(Scene scene) {
		insertSceneDevices(scene, scene.getZone());
	}

	private void insertSceneDevices(Scene scene, Zone zone) {
		List<Device> devices = zone.getDevices();
		if (devices == null) {
			return;
		}
		for (Device device : devices) {
			insertSceneDevice(scene, device);
		}
	}

	private void insertSceneDevice(Scene scene, Device device) {
		SceneDevice sceneDevice = new SceneDevice();
		sceneDevice.setScene(scene);
		sceneDevice.setDevice(device);
		sceneDeviceService.insert(sceneDevice);
	}

	@Override
	public List<SceneDto> findDtoByUserZone(User user, long zoneId) {
		List<Scene> scenes = getDao().findByUserZoneId(user.getId(), zoneId);
		return sceneMapper.createSceneDtoList(scenes);
	}

	@Override
	public List<SceneDto> findDtoByProjectId(long projectId) {
		List<Scene> scenes = getDao().findByProjectId(projectId);
		return sceneMapper.createSceneDtoList(scenes);
	}

	@Override
	public SceneDto requireDtoById(long id) {
		return sceneMapper.createSceneDto(requireById(id));
	}

	@Override
	public SceneDto insertDto(User user, SceneDto dto) {
		Scene scene = sceneMapper.createScene(dto, zoneService);
		insert(scene);
		return sceneMapper.createSceneDto(scene);
	}

	@Override
	public SceneDto updateDto(User user, SceneDto dto) {
		final Scene scene = requireById(dto.getId());
		sceneMapper.updateScene(scene, dto);
		update(scene);
		return sceneMapper.createSceneDto(scene);
	}

	@Override
	public void deleteDtoById(User user, long id) {
		final Scene scene = requireById(id);
		delete(scene);
	}

	@Override
	public List<SceneDto> findDtoByUserProject(User user, Long projectId) {
		List<Scene> scenes = getDao().findByUserProjectId(user.getId(), projectId); 
		return sceneMapper.createSceneDtoList(scenes);
	}

	@Override
	protected List<SceneDto> createDtoList(List<Scene> entities) {
		return sceneMapper.createSceneDtoList(entities);
	}

	@Override
	public SceneDto finDtoById(Long sceneId) {
		final Scene scene = requireById(sceneId);
		SceneDto sceneDto = sceneMapper.createSceneDto(scene);
		sceneDto.setDevices(new ArrayList<>());
		if (scene.getDevices() != null) {
			for (SceneDevice sceneDevice : scene.getDevices()) {
				sceneDto.getDevices().add(sceneMapper.createSceneDeviceDto(sceneDevice));
			}
		}
		return sceneDto;
	}
}
