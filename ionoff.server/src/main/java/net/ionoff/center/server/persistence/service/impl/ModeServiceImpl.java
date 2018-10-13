package net.ionoff.center.server.persistence.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Mode;
import net.ionoff.center.server.entity.ModeScene;
import net.ionoff.center.server.entity.Project;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.server.persistence.mapper.ModeMapper;
import net.ionoff.center.server.persistence.dao.IModeDao;
import net.ionoff.center.server.persistence.dao.IModeSceneDao;
import net.ionoff.center.server.persistence.service.IModeService;
import net.ionoff.center.server.persistence.service.IProjectService;
import net.ionoff.center.shared.dto.ModeDto;

@Service
@Transactional
public class ModeServiceImpl extends AbstractGenericService<Mode, ModeDto> implements IModeService {

	@Autowired
	private ModeMapper modeMapper;
	
	@Autowired
	private IModeSceneDao modeSceneDao;

	@Autowired
	private IProjectService projectService;
	
	private IModeDao modeDao;

	@Autowired
	public ModeServiceImpl(IModeDao modeDao) {
		this.modeDao = modeDao;
	}

	@Override
	protected IModeDao getDao() {
		return modeDao;
	}
	
	@Override
	public Mode insert(Mode mode) {
		super.insert(mode);
		insertModeScenes(mode);
		return mode;
	}

	private void insertModeScenes(Mode mode) {
		Project project = mode.getProject();
		if (project.getZones() == null) {
			return;
		}
		for (Zone zone : project.getZones()) {
			insertModeScene(mode, zone);
		}
	}

	private void insertModeScene(Mode mode, Zone zone) {
		ModeScene modeScene = new ModeScene();
		modeScene.setMode(mode);
		modeScene.setZone(zone);
		modeSceneDao.insert(modeScene);
	}

	@Override
	public List<Mode> findByProjectId(long projectId) {
		return getDao().findByProjectId(projectId);
	}

	@Override
	public List<Mode> findByScheduleTime(String scheduleTime) {
		return getDao().findByScheduleTime(scheduleTime);
	}

	@Override
	public List<ModeDto> findDtoByProjectId(long projectId) {
		List<Mode> modes = findByProjectId(projectId);
		return modeMapper.createModeDtoList(modes);
	}

	@Override
	public ModeDto insertDto(User user, ModeDto dto) {
		Mode mode = modeMapper.createMode(dto, projectService);
		insert(mode);
		return modeMapper.createModeDto(mode);
	}

	@Override
	public ModeDto updateDto(User user, ModeDto dto) {
		Mode mode = requireById(dto.getId());
		modeMapper.updateMode(mode, dto);
		update(mode);
		return modeMapper.createModeDto(mode);
	}

	@Override
	public ModeDto requireDtoById(long id) {
		return modeMapper.createModeDto(requireById(id));
	}

	@Override
	public ModeDto findActivatedDtoByProjectId(long projectId) {
		Mode activatedMode = modeDao.findByLastActivated(projectId);
		if (activatedMode != null) {
			return modeMapper.createModeDto(activatedMode);
		}
		return null;
	}

	@Override
	public void deleteDtoById(User user, long id) {
		Mode mode = requireById(id);
		delete(mode);
	}

	@Override
	protected List<ModeDto> createDtoList(List<Mode> entities) {
		return modeMapper.createModeDtoList(entities);
	}
}
