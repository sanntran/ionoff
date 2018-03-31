package net.ionoff.center.server.persistence.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.server.entity.ModeSensorScene;
import net.ionoff.center.server.entity.ModeSensorUser;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.entity.UserProject;
import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.server.objmapper.ModeMapper;
import net.ionoff.center.server.persistence.dao.IModeSensorDao;
import net.ionoff.center.server.persistence.dao.IModeSensorSceneDao;
import net.ionoff.center.server.persistence.dao.IModeSensorUserDao;
import net.ionoff.center.server.persistence.service.IModeSensorService;
import net.ionoff.center.shared.dto.ModeSensorDto;

@Transactional
public class ModeSensorServiceImpl extends AbstractGenericService<ModeSensor, ModeSensorDto> implements IModeSensorService {

	private IModeSensorDao modeSensorDao;
	
	@Autowired
	private ModeMapper modeMapper;
	
	@Autowired
	private IModeSensorSceneDao modeSensorSceneDao;
	
	@Autowired
	private IModeSensorUserDao modeSensorUserDao;
	
	public ModeSensorServiceImpl(IModeSensorDao modeSensorDao) {
		this.modeSensorDao = modeSensorDao;
	}

	@Override
	protected IModeSensorDao getDao() {
		return modeSensorDao;
	}
	
	@Override
	public ModeSensor insert(ModeSensor modeSensor) {
		super.insert(modeSensor);
		insertModeSensorScenes(modeSensor);
		insertModeSensorUsers(modeSensor);
		return modeSensor;
	}

	private void insertModeSensorScenes(ModeSensor modeSensor) {
		if (modeSensor.getMode().getProject().getZones() == null) {
			return;
		}
		for (Zone zone : modeSensor.getMode().getProject().getZones()) {
			insertModeSensorScene(modeSensor, zone, false);
			insertModeSensorScene(modeSensor, zone, true);
		}
	}
	
	private void insertModeSensorUsers(ModeSensor modeSensor) {
		for (UserProject userProject : modeSensor.getMode().getProject().getUsers()) {
			if (userProject.hasRole()) {
				insertModeSensorUser(modeSensor, userProject.getUser(), false);
				insertModeSensorUser(modeSensor, userProject.getUser(), true);
			}
		}
	}

	private void insertModeSensorUser(ModeSensor modeSensor, User user, boolean detected) {
		ModeSensorUser modeSensorUser = new ModeSensorUser();
		modeSensorUser.setDetected(detected);
		modeSensorUser.setModeSensor(modeSensor);
		modeSensorUser.setUser(user);
		modeSensorUser.setSendEmail(false);
		modeSensorUser.setSendSms(false);
		modeSensorUser.setProject(modeSensor.getMode().getProject());
		modeSensorUserDao.insert(modeSensorUser);
	}

	private void insertModeSensorScene(ModeSensor modeSensor, Zone zone, boolean detected) {
		ModeSensorScene modeSensorScene = new ModeSensorScene();
		modeSensorScene.setDetected(detected);
		modeSensorScene.setModeSensor(modeSensor);
		modeSensorScene.setZone(zone);
		modeSensorSceneDao.insert(modeSensorScene);
	}

	@Override
	public ModeSensorDto insertDto(User user, ModeSensorDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public ModeSensorDto updateDto(User user, ModeSensorDto dto) {
		
		final ModeSensor modeSensor = requireById(dto.getId());
		modeSensor.setEnabled(dto.getEnabled());
		modeSensor.setTimeBuffer(dto.getTimeBuffer());
		update(modeSensor);
		return modeMapper.createModeSensorDto(modeSensor);
	}

	@Override
	public ModeSensorDto requireDtoById(long id) {
		return modeMapper.createModeSensorDto(requireById(id));
	}

	@Override
	public void deleteDtoById(User user, long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected List<ModeSensorDto> createDtoList(List<ModeSensor> entities) {
		throw new UnsupportedOperationException();
	}
}
