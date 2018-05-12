package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Mode;
import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.server.entity.ModeSensorScene;
import net.ionoff.center.server.entity.ModeSensorUser;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.entity.UserProject;
import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.server.objmapper.ModeMapper;
import net.ionoff.center.server.persistence.dao.IModeSensorDao;
import net.ionoff.center.server.persistence.dao.IModeSensorSceneDao;
import net.ionoff.center.server.persistence.dao.IModeSensorUserDao;
import net.ionoff.center.server.persistence.service.IModeSensorService;
import net.ionoff.center.server.persistence.service.IModeService;
import net.ionoff.center.server.persistence.service.ISensorService;
import net.ionoff.center.shared.dto.ModeSensorDto;

@Transactional
public class ModeSensorServiceImpl extends AbstractGenericService<ModeSensor, ModeSensorDto> implements IModeSensorService {

	private IModeSensorDao modeSensorDao;
	
	@Autowired
	private ModeMapper modeMapper;
	
	@Autowired
	private ISensorService sensorService;
	
	@Autowired
	private IModeService modeService;
	
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
		modeSensor.setScenes(new HashSet<>());
		if (modeSensor.getSensor().getProject().getZones() == null) {
			return;
		}
		for (Zone zone : modeSensor.getSensor().getProject().getZones()) {
			insertModeSensorScene(modeSensor, zone);
		}
	}
	
	private void insertModeSensorUsers(ModeSensor modeSensor) {
		modeSensor.setUsers(new HashSet<>());
		for (UserProject userProject : modeSensor.getSensor().getProject().getUsers()) {
			if (userProject.hasRole()) {
				insertModeSensorUser(modeSensor, userProject.getUser());
			}
		}
	}

	private void insertModeSensorUser(ModeSensor modeSensor, User user) {
		ModeSensorUser modeSensorUser = new ModeSensorUser();
		modeSensorUser.setModeSensor(modeSensor);
		modeSensorUser.setUser(user);
		modeSensorUser.setSendEmail(false);
		modeSensorUser.setSendSms(false);
		modeSensorUser.setProject(modeSensor.getSensor().getProject());
		modeSensorUserDao.insert(modeSensorUser);
		modeSensor.getUsers().add(modeSensorUser);
	}

	private void insertModeSensorScene(ModeSensor modeSensor, Zone zone) {
		ModeSensorScene modeSensorScene = new ModeSensorScene();
		modeSensorScene.setModeSensor(modeSensor);
		modeSensorScene.setZone(zone);
		modeSensorSceneDao.insert(modeSensorScene);
		modeSensor.getScenes().add(modeSensorScene);
	}

	@Override
	public ModeSensorDto insertDto(User user, ModeSensorDto modeSensorDto) {
		final ModeSensor modeSensor = createModeSensor(modeSensorDto);
		insert(modeSensor);
		return modeMapper.createModeSensorDto(modeSensor);
	}

	private ModeSensor createModeSensor(ModeSensorDto modeSensorDto) {
		ModeSensor modeSensor = new ModeSensor();
		modeSensor.setEnabled(modeSensorDto.getEnabled());
		Sensor sensor = sensorService.findById(modeSensorDto.getSensorId());
		modeSensor.setSensor(sensor);
		Mode mode = null;
		if (modeSensorDto.getModeId() != null) {
			mode = modeService.findById(modeSensorDto.getModeId());
		}
		modeSensor.setMode(mode);
		return modeSensor;
	}

	@Override
	public ModeSensorDto updateDto(User user, ModeSensorDto dto) {
		final ModeSensor modeSensor = requireById(dto.getId());
		modeSensor.setEnabled(dto.getEnabled());
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

	@Override
	public List<ModeSensorDto> findDtosBySensorId(Long sensorId) {
		List<ModeSensor> modeSensors = modeSensorDao.findBySensorId(sensorId);
		List<ModeSensorDto> modeSensorDtos = new ArrayList<>();
		for (ModeSensor modeSensor : modeSensors) {
			modeSensorDtos.add(modeMapper.createModeSensorDto(modeSensor));
		}
		return modeSensorDtos;
	}
}
