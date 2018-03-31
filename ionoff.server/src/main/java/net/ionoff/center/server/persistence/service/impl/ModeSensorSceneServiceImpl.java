package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.server.entity.ModeSensorScene;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.objmapper.ModeMapper;
import net.ionoff.center.server.persistence.dao.IModeSensorSceneDao;
import net.ionoff.center.server.persistence.service.IModeSensorSceneService;
import net.ionoff.center.server.persistence.service.IModeSensorService;
import net.ionoff.center.server.persistence.service.ISceneService;
import net.ionoff.center.shared.dto.ModeSensorSceneDto;

@Transactional
public class ModeSensorSceneServiceImpl extends AbstractGenericService<ModeSensorScene, ModeSensorSceneDto> implements IModeSensorSceneService {
	
	@Autowired
	private ModeMapper modeMapper;
	
	@Autowired
	private ISceneService sceneService;
	
	@Autowired
	private IModeSensorService modeSensorService;
	
	private IModeSensorSceneDao modeSensorSceneDao;
	
	public ModeSensorSceneServiceImpl(IModeSensorSceneDao modeSensorSceneDao) {
		this.modeSensorSceneDao = modeSensorSceneDao;
	}

	@Override
	protected IModeSensorSceneDao getDao() {
		return modeSensorSceneDao;
	}

	@Override
	public ModeSensorSceneDto insertDto(User user, ModeSensorSceneDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public ModeSensorSceneDto updateDto(User user, ModeSensorSceneDto dto) {
		final ModeSensorScene modeSensorScene = requireById(dto.getId());
		if (dto.getSceneId() != null) {
			modeSensorScene.setScene(sceneService.findById(dto.getSceneId()));
		}
		else {
			modeSensorScene.setScene(null);
		}
		update(modeSensorScene);
		return modeMapper.createModeSensorSceneDto(modeSensorScene);
	}

	@Override
	public ModeSensorSceneDto requireDtoById(long id) {
		return modeMapper.createModeSensorSceneDto(requireById(id));
	}

	@Override
	public List<ModeSensorSceneDto> findByModeSensorId(Long modeSensorId, boolean detected) {
		final ModeSensor modeSensor = modeSensorService.requireById(modeSensorId);
		final List<ModeSensorSceneDto> response = new ArrayList<ModeSensorSceneDto>();
		for (final ModeSensorScene modeSensorScene : modeSensor.getScenes()) {
			if (detected == modeSensorScene.getDetected()) {
				response.add(modeMapper.createModeSensorSceneDto(modeSensorScene));
			}
		}
		return response;
	}

	@Override
	public void deleteDtoById(User user, long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected List<ModeSensorSceneDto> createDtoList(List<ModeSensorScene> entities) {
		throw new UnsupportedOperationException();
	}
}
