package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.hibernate.Cache;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Dashboard;
import net.ionoff.center.server.entity.DashboardDevice;
import net.ionoff.center.server.entity.DashboardScene;
import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.Scene;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.EntityNotFoundException;
import net.ionoff.center.server.persistence.mapper.DashboardMapper;
import net.ionoff.center.server.persistence.mapper.DeviceMapper;
import net.ionoff.center.server.persistence.dao.IDashboardDao;
import net.ionoff.center.server.persistence.dao.IDashboardDeviceDao;
import net.ionoff.center.server.persistence.dao.IDashboardSceneDao;
import net.ionoff.center.server.persistence.dao.IDeviceDao;
import net.ionoff.center.server.persistence.dao.ISceneDao;
import net.ionoff.center.server.persistence.service.IDashboardService;
import net.ionoff.center.shared.dto.DashboardDto;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.server.mediaplayer.service.IMediaPlayerService;

@Service
@Transactional
public class DashboardServiceImpl extends AbstractGenericService<Dashboard, DashboardDto> implements IDashboardService {
	
	private IDashboardDao dashboardDao;
	
	@Autowired
	private DashboardMapper dashboardMapper;
	
	@Autowired
	private IDeviceDao deviceDao;
	
	@Autowired
	private ISceneDao sceneDao;
	
	@Autowired
	private IDashboardDeviceDao dashboardDeviceDao;
	
	@Autowired
	private IDashboardSceneDao dashboardSceneDao;
	
	@Autowired
	private DeviceMapper deviceMapper;
	
	@Autowired
	private IMediaPlayerService playerService;

	@Autowired
	public DashboardServiceImpl(IDashboardDao dashboardDao) {
		this.dashboardDao = dashboardDao;
	}
	
	@Override
	protected IDashboardDao getDao() {
		return dashboardDao;
	}

	@Override
	public DashboardDto requireDtoById(long id) {
		Dashboard dashboard = dashboardDao.findById(id);
		return dashboardMapper.createDto(dashboard);
	}

	@Override
	public DashboardDto insertDto(User user, DashboardDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public DashboardDto updateDto(User user, DashboardDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void deleteDtoById(User user, long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Dashboard findByUserZoneId(User user, long zoneId) {
		return dashboardDao.findByUserZoneId(user.getId(), zoneId);
	}

	@Override
	public Dashboard findByUserProjectId(User user, long projectId) {
		return dashboardDao.findByUserProjectId(user.getId(), projectId);
	}

	@Override
	protected List<DashboardDto> createDtoList(List<Dashboard> entities) {
		List<DashboardDto> dashboardDtos = new ArrayList<DashboardDto>();
		for (Dashboard dashboard : entities) {
			dashboardDtos.add(dashboardMapper.createDto(dashboard));
		}
		return dashboardDtos;
	}

	@Override
	public void removeByUserProject(User user, long projectId) {
		dashboardDao.removeByUserProjectId(user.getId(), projectId);
	}

	@Override
	public DashboardDto findDtoByUserZoneId(User user, long zoneId) {
		Dashboard dashboard = findByUserZoneId(user, zoneId);
		DashboardDto dashboardDto = dashboardMapper.createDto(dashboard);
		List<DeviceDto> deviceDtos = new ArrayList<>();
		for (DashboardDevice dashboardDevice : dashboard.getDevices()) {
			Device device = dashboardDevice.getDevice();
			deviceDtos.add(deviceMapper.createDeviceDto(device, playerService));
		}
		dashboardDto.setDevices(deviceDtos);
		return dashboardDto;
	}

	@Override
	public DashboardDto findDtoByUserProjectId(User user, long projectId) {
		Dashboard dashboard = findByUserProjectId(user, projectId);
		DashboardDto dashboardDto = dashboardMapper.createDto(dashboard);
		List<DeviceDto> deviceDtos = new ArrayList<>();
		for (DashboardDevice dashboardDevice : dashboard.getDevices()) {
			Device device = dashboardDevice.getDevice();
			deviceDtos.add(deviceMapper.createDeviceDto(device, playerService));
		}
		dashboardDto.setDevices(deviceDtos);
		return dashboardDto;
	}

	@Override
	public void addDeviceToZoneDashboard(User user, Long deviceId) {
		Device device = requireDeviceById(deviceId);
		Dashboard dashboard = dashboardDao.findByUserZoneId(user.getId(), device.getZone().getId());
		for (DashboardDevice dashboardDevice : dashboard.getDevices()) {
			if (dashboardDevice.getDevice().getId() == deviceId) {
				return;
			}
		}
		DashboardDevice dashboardDevice = new DashboardDevice();
		dashboardDevice.setDashboard(dashboard);
		dashboardDevice.setDevice(device);
		dashboardDevice.setProject(device.getProject());
		dashboardDeviceDao.insert(dashboardDevice);
		Cache cache = dashboardDeviceDao.getSessionFactory().getCache();
		if (cache != null) {
		    cache.evictAllRegions();
		}
	}
	
	@Override
	public void removeDeviceFromZoneDashboard(User user, Long deviceId) {
		Device device = requireDeviceById(deviceId);
		Dashboard dashboard = dashboardDao.findByUserZoneId(user.getId(), device.getZone().getId());
		DashboardDevice dashboardDevice = dashboardDeviceDao.findByDashboardDeviceId(dashboard.getId(), device.getId());
		if (dashboardDevice != null) {
			dashboardDeviceDao.delete(dashboardDevice);
			Cache cache = dashboardDeviceDao.getSessionFactory().getCache();
			if (cache != null) {
			    cache.evictAllRegions();
			}
		}
	}

	@Override
	public void addDeviceToProjectDashboard(User user, Long deviceId) {
		Device device = requireDeviceById(deviceId);
		Dashboard dashboard = dashboardDao.findByUserProjectId(user.getId(), device.getProject().getId());
		for (DashboardDevice dashboardDevice : dashboard.getDevices()) {
			if (dashboardDevice.getDevice().getId() == deviceId) {
				return;
			}
		}
		DashboardDevice dashboardDevice = new DashboardDevice();
		dashboardDevice.setDashboard(dashboard);
		dashboardDevice.setDevice(device);
		dashboardDevice.setProject(device.getProject());
		dashboardDeviceDao.insert(dashboardDevice);
		Cache cache = dashboardDeviceDao.getSessionFactory().getCache();
		if (cache != null) {
		    cache.evictAllRegions();
		}
	}
	
	@Override
	public void removeDeviceFromProjectDashboard(User user, Long deviceId) {
		Device device = requireDeviceById(deviceId);
		Dashboard dashboard = dashboardDao.findByUserProjectId(user.getId(), device.getProject().getId());
		DashboardDevice dashboardDevice = dashboardDeviceDao.findByDashboardDeviceId(dashboard.getId(), device.getId());
		if (dashboardDevice != null) {
			dashboardDeviceDao.delete(dashboardDevice);
			Cache cache = dashboardDeviceDao.getSessionFactory().getCache();
			if (cache != null) {
			    cache.evictAllRegions();
			}
		}
	}
	
	@Override
	public void addSceneToZoneDashboard(User user, Long sceneId) {
		Scene scene = requireSceneById(sceneId);
		Dashboard dashboard = dashboardDao.findByUserZoneId(user.getId(), scene.getZone().getId());
		DashboardScene dashboardScene = new DashboardScene();
		dashboardScene.setDashboard(dashboard);
		dashboardScene.setScene(scene);
		dashboardScene.setProject(scene.getZone().getProject());
		dashboardSceneDao.insert(dashboardScene);
		Cache cache = dashboardDeviceDao.getSessionFactory().getCache();
		if (cache != null) {
		    cache.evictAllRegions();
		}
	}
	
	@Override
	public void removeSceneFromZoneDashboard(User user, Long sceneId) {
		Scene scene = requireSceneById(sceneId);
		Dashboard dashboard = dashboardDao.findByUserZoneId(user.getId(), scene.getZone().getId());
		DashboardScene dashboardScene = dashboardSceneDao.findByDashboardSceneId(dashboard.getId(), scene.getId());
		if (dashboardScene != null) {
			dashboardSceneDao.delete(dashboardScene);
			Cache cache = dashboardDeviceDao.getSessionFactory().getCache();
			if (cache != null) {
			    cache.evictAllRegions();
			}
		}
	}

	@Override
	public void addSceneToProjectDashboard(User user, Long sceneId) {
		Scene scene = requireSceneById(sceneId);
		Dashboard dashboard = dashboardDao.findByUserProjectId(user.getId(), scene.getZone().getProject().getId());
		DashboardScene dashboardScene = new DashboardScene();
		dashboardScene.setDashboard(dashboard);
		dashboardScene.setScene(scene);
		dashboardScene.setProject(scene.getZone().getProject());
		dashboardSceneDao.insert(dashboardScene);
		Cache cache = dashboardDeviceDao.getSessionFactory().getCache();
		if (cache != null) {
		    cache.evictAllRegions();
		}
	}
	
	@Override
	public void removeSceneFromProjectDashboard(User user, Long sceneId) {
		Scene scene = requireSceneById(sceneId);
		Dashboard dashboard = dashboardDao.findByUserProjectId(user.getId(), scene.getZone().getProject().getId());
		DashboardScene dashboardScene = dashboardSceneDao.findByDashboardSceneId(dashboard.getId(), scene.getId());
		if (dashboardScene != null) {
			dashboardSceneDao.delete(dashboardScene);
			Cache cache = dashboardDeviceDao.getSessionFactory().getCache();
			if (cache != null) {
			    cache.evictAllRegions();
			}
		}
	}
	
	private Device requireDeviceById(Long deviceId) {
		Device d = deviceDao.findById(deviceId);
		if (d == null) {
			throw new EntityNotFoundException(deviceId, Device.class.getSimpleName());
		}
		return d;
	}

	private Scene requireSceneById(Long sceneId) {
		Scene s = sceneDao.findById(sceneId);
		if (s == null) {
			throw new EntityNotFoundException(sceneId, Device.class.getSimpleName());
		}
		return s;
	}

}
