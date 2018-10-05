package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Area;
import net.ionoff.center.server.entity.Dashboard;
import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.Light;
import net.ionoff.center.server.entity.Mode;
import net.ionoff.center.server.entity.ModeScene;
import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.server.entity.ModeSensorScene;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.entity.UserProject;
import net.ionoff.center.server.entity.UserZone;
import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.locale.Constants;
import net.ionoff.center.server.locale.Messages;
import net.ionoff.center.server.persistence.mapper.ZoneMapper;
import net.ionoff.center.server.persistence.dao.IAreaDao;
import net.ionoff.center.server.persistence.dao.IDashboardDao;
import net.ionoff.center.server.persistence.dao.IDeviceDao;
import net.ionoff.center.server.persistence.dao.IModeSceneDao;
import net.ionoff.center.server.persistence.dao.IModeSensorSceneDao;
import net.ionoff.center.server.persistence.dao.IZoneDao;
import net.ionoff.center.server.persistence.service.IUserZoneService;
import net.ionoff.center.server.persistence.service.IZoneService;
import net.ionoff.center.shared.dto.ZoneDto;

@Transactional
public class ZoneServiceImpl extends AbstractGenericService<Zone, ZoneDto> implements IZoneService {

	private IZoneDao zoneDao;
	
	@Autowired
	private IAreaDao areaDao;
	
	@Autowired
	private IModeSceneDao modeSceneDao;
	
	@Autowired
	private IModeSensorSceneDao modeSensorSceneDao;
	
	@Autowired
	private IUserZoneService userZoneService;
	
	@Autowired
	private IDashboardDao dashboardDao;

	@Autowired
	private ZoneMapper zoneMapper;
	
	@Autowired
	private IDeviceDao deviceDao;
	
	public ZoneServiceImpl(IZoneDao zoneDao) {
		this.zoneDao = zoneDao;
	}

	@Override
	protected IZoneDao getDao() {
		return zoneDao;
	}
	
	
	@Override
	public Zone insert(Zone entity) {
		super.insert(entity);
		insertUserZones(entity);
		insertUserDashboards(entity);
		insertModeScenes(entity);
		insertModeSensorScenes(entity);
		return entity;
	}
	
	private void insertUserDashboards(Zone entity) {
		for (UserProject userProject : entity.getProject().getUsers()) {
			if (userProject.hasRole()) {
				Dashboard dashboard = new Dashboard();
				dashboard.setUser(userProject.getUser());
				dashboard.setZone(entity);
				dashboard.setProject(entity.getProject());
				dashboardDao.insert(dashboard);
			}
		}
	}

	private void insertUserZones(Zone entity) {
		for (UserProject userProject : entity.getProject().getUsers()) {
			if (userProject.hasRole()) {
				UserZone userZone = new UserZone();
				userZone.setUser(userProject.getUser());
				userZone.setZone(entity);
				userZone.setProject(entity.getProject());
				if (userProject.getUser().hasAdminRole()) {
					userZone.setRole(true);
				}
				userZoneService.insert(userZone);
			}
		}
	}

	private void insertModeSensorScenes(Zone zone) {
		if (zone.getProject().getModes() == null) {
			return;
		}
		for (Mode mode : zone.getProject().getModes()) {
			insertModeSensorScenes(mode, zone);
		}
	}

	private void insertModeSensorScenes(Mode mode, Zone zone) {
		if (mode.getSensors() == null) {
			return;
		}
		for (ModeSensor modeSensor : mode.getSensors()) {
			insertModeSensorScene(modeSensor, zone);
		}
	}

	private void insertModeSensorScene(ModeSensor modeSensor, Zone zone) {
		ModeSensorScene modeSensorScene = new ModeSensorScene();
		modeSensorScene.setModeSensor(modeSensor);
		modeSensorScene.setZone(zone);
		modeSensorSceneDao.insert(modeSensorScene);
	}

	private void insertModeScenes(Zone zone) {
		if (zone.getProject().getModes() == null) {
			return;
		}
		for (Mode mode : zone.getProject().getModes()) {
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
	public List<Zone> findByProjectId(long projectId) {
		return getDao().findByProjectId(projectId);
	}

	@Override
	public List<Zone> findByAreaId(long areaId) {
		return getDao().findByAreaId(areaId);
	}

	@Override
	public void update(Zone zone, Long areaId) {
		if (zone.getArea().getId() == areaId.longValue()) {
			update(zone);
		}
		else {
			// change area
			Area area = areaDao.findById(areaId);
			zone.setArea(area);
			zone.setProject(area.getProject());
			update(zone);
		}
	}

	@Override
	public ZoneDto requireDtoById(long id) {
		return zoneMapper.createZoneDto(requireById(id), false);
	}

	@Override
	public ZoneDto insertDto(User user, ZoneDto dto) {
		validateZone(dto, user.getLanguage());
		final Zone zone = zoneMapper.createZone(dto);
		insert(zone);
		return zoneMapper.createZoneDto(zone, false);
	}

	private void validateZone(ZoneDto zoneDto, String language) {
		if (zoneDto.getAreaId() == null) {
			final String message = Messages.get(language)
					.fieldInvalid(Constants.get(language).area(), "null");
			throw new UpdateEntityException(message);
		}
	}

	@Override
	public ZoneDto updateDto(User user, ZoneDto dto) {
		validateZone(dto, user.getLanguage());
		final Zone zone = requireById(dto.getId());
		if (zone.getArea().getId() != dto.getAreaId().longValue()) {
			zoneMapper.updateZone(zone, dto);
			update(zone, dto.getAreaId());
		}
		else {
			zoneMapper.updateZone(zone, dto);
			update(zone);
		}
		return zoneMapper.createZoneDto(zone, false);
	}

	@Override
	public void deleteDtoById(User user, long id) {
		Zone zone = requireById(id);
		if (zone != null && zone.getDevices() != null && !zone.getDevices().isEmpty()) {
			final String entity = Constants.get(user.getLanguage()).zone();
			final String ownedEntity = Constants.get(user.getLanguage()).device();
			final String message = Messages.get(user.getLanguage())
					.errorDeleteNotEmptyEntity(entity, ownedEntity);
			throw new DeleteEntityException(message);
		}
		delete(zone);
	}

	@Override
	public List<ZoneDto> findDtoByUserProjectId(long userId, long projectId) {
		List<ZoneDto> zoneDtos = new ArrayList<>();
		for (Zone zone : zoneDao.findByUserProjectId(userId, projectId)) {
			ZoneDto zoneDto = zoneMapper.createZoneDto(zone, false);
			List<Device> devices = deviceDao.findByUserZoneId(userId, zone.getId());
			for (Device device : devices) {
				if (device instanceof Light && Boolean.TRUE.equals(device.getStatus())) {
					zoneDto.setLighting(true);
					break;
				}
			}
			zoneDto.setDevicesCount(devices.size());
			zoneDtos.add(zoneDto);
		}
		return zoneDtos;
	}

	@Override
	protected List<ZoneDto> createDtoList(List<Zone> entities) {
		return zoneMapper.createZoneDtoList(entities, false);
	}
}
