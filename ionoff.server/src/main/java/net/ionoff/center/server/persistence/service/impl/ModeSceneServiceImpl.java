package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Mode;
import net.ionoff.center.server.entity.ModeScene;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.persistence.mapper.ModeMapper;
import net.ionoff.center.server.persistence.dao.IModeSceneDao;
import net.ionoff.center.server.persistence.service.IModeSceneService;
import net.ionoff.center.server.persistence.service.IModeService;
import net.ionoff.center.server.persistence.service.ISceneService;
import net.ionoff.center.shared.dto.ModeSceneDto;

@Transactional
public class ModeSceneServiceImpl extends AbstractGenericService<ModeScene, ModeSceneDto> implements IModeSceneService {
	
	@Autowired
	private ModeMapper modeMapper;
	
	@Autowired
	private ISceneService sceneService;
	
	@Autowired
	private IModeService modeService;
	
	private IModeSceneDao modeSceneDao;
	
	
	public ModeSceneServiceImpl(IModeSceneDao modeSceneDao) {
		this.modeSceneDao = modeSceneDao;
	}

	@Override
	protected IModeSceneDao getDao() {
		return modeSceneDao;
	}

	@Override
	public ModeSceneDto insertDto(User user, ModeSceneDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public ModeSceneDto updateDto(User user, ModeSceneDto dto) {
		final ModeScene modeScene = requireById(dto.getId());
		if (dto.getSceneId() != null) {
			modeScene.setScene(sceneService.findById(dto.getSceneId()));
		}
		else {
			modeScene.setScene(null);
		}
		update(modeScene);
		return modeMapper.createModeSceneDto(modeScene);
	}

	@Override
	public ModeSceneDto requireDtoById(long id) {
		return modeMapper.createModeSceneDto(requireById(id));
	}

	@Override
	public List<ModeSceneDto> findByModeId(Long modeId) {
		final Mode mode = modeService.findById(modeId);
		if (mode.getScenes() == null || mode.getScenes().isEmpty()) {
			return Collections.emptyList();
		}
		List<ModeSceneDto> modeSceneDtos = new ArrayList<>();
		for (final ModeScene modeScene : mode.getScenes()) {
			modeSceneDtos.add(modeMapper.createModeSceneDto(modeScene));
		}
		return modeSceneDtos;
	}

	@Override
	public void deleteDtoById(User user, long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected List<ModeSceneDto> createDtoList(List<ModeScene> entities) {
		throw new UnsupportedOperationException();
	}
}
