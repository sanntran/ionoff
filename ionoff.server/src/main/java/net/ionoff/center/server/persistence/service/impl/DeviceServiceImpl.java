package net.ionoff.center.server.persistence.service.impl;

import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.EntityUtil;
import net.ionoff.center.server.entity.Player;
import net.ionoff.center.server.entity.Scene;
import net.ionoff.center.server.entity.SceneDevice;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.SensorDriver;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.entity.UserDevice;
import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.server.mediaplayer.model.MediaPlayer;
import net.ionoff.center.server.mediaplayer.service.IMediaPlayerService;
import net.ionoff.center.server.message.SensorStatusNotifier;
import net.ionoff.center.server.message.event.SensorStatusChangedEvent;
import net.ionoff.center.server.persistence.dao.IDeviceDao;
import net.ionoff.center.server.persistence.dao.IUserDao;
import net.ionoff.center.server.persistence.dao.IUserDeviceDao;
import net.ionoff.center.server.persistence.dao.IZoneDao;
import net.ionoff.center.server.persistence.mapper.DeviceMapper;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.persistence.service.ISceneDeviceService;
import net.ionoff.center.server.persistence.service.ISensorService;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.StatusDto;
import net.ionoff.center.shared.entity.SensorType;
import org.hibernate.Cache;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@EnableAsync
@Transactional
public class DeviceServiceImpl extends AbstractGenericService<Device, DeviceDto> implements IDeviceService {

	private IDeviceDao deviceDao;
	
	@Autowired
	private IUserDao userDao;
	
	@Autowired
	private IUserDeviceDao userDeviceDao;
	
	@Autowired
	private IZoneDao zoneDao;
	
	@Autowired
	private DeviceMapper deviceMapper;

	@Autowired
	protected ISensorService sensorService;
	
	@Autowired
	private ISceneDeviceService sceneDeviceService;
	
	@Autowired 
	private IMediaPlayerService playerService;

	@Autowired
	private SensorStatusNotifier sensorStatusNotifier;
	
	public DeviceServiceImpl(IDeviceDao deviceDao) {
		this.deviceDao = deviceDao;
	}

	@Override
	protected IDeviceDao getDao() {
		return deviceDao;
	}
	
	protected IUserDao getUserDao() {
		return userDao;
	}
	
	@Override
	public Device insert(Device device) {
		super.insert(device);
		insertSceneDevices(device);
		insertUserDevices(device);
		if (device instanceof SensorDriver) {
			Sensor sensor = new Sensor();
			sensor.setDevice(device);
			sensor.setName("Weigh Sensor");
			sensor.setProject(device.getProject());
			sensor.setType(SensorType.ANALOG.toString());
			sensor.setUnit("kg");
			sensor.setZone(device.getZone());
			sensorService.insert(sensor);
		}
		Cache cache = getDao().getSessionFactory().getCache();
		if (cache != null) {
		    cache.evictAllRegions();
		}
		
		return device;
	}

	private void insertUserDevices(Device device) {
		List<User> users = userDao.findByProjectId(device.getProject().getId());
		for (User user : users) { 
			UserDevice userDevice = new UserDevice();
			userDevice.setUser(user);
			userDevice.setDevice(device);
			if (user.hasAdminRole()) {
				userDevice.setRole(true);
			}
			else {
				userDevice.setRole(false);
			}
			userDevice.setProject(device.getProject());
			userDeviceDao.insert(userDevice);
		}
	}

	@Override
	public void moveDevice(Device device, Zone fromZone, Zone toZone) {
		if (fromZone.getScenes() != null) {
			for (Scene scene : fromZone.getScenes()) {
				removeSceneDevice(scene, device);
			}
		}
		if (toZone.getScenes() != null) {
			for (Scene scene : toZone.getScenes()) {
				insertSceneDevice(scene, device);
			}
		}
	}
	
	private void removeSceneDevice(Scene scene, Device device) {
		SceneDevice sceneDevice = sceneDeviceService.findBySceneIdDeviceId(scene.getId(), device.getId());
		sceneDeviceService.delete(sceneDevice);
	}

	@Override
	public void update(Device device, Long zoneId) {
		if (device.getZone().getId() == zoneId) {
			super.update(device);
		}
		else {	
			final Zone fromZone = device.getZone();
			final Zone toZone = zoneDao.findById(zoneId);
			device.setZone(toZone);
			device.setProject(toZone.getProject());
			super.update(device);
			//
			if (fromZone.getId() != toZone.getId()) {
				// zone changed
				moveDevice(device, fromZone, toZone);
			}
		}
	}
	
	private void insertSceneDevices(Device device) {
		Zone zone = device.getZone();
		if (zone.getScenes() == null) {
			return;
		}
		for (Scene scene : zone.getScenes()) {
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
	public Player findPlayerByMac(String mac) {
		return getDao().findPlayerByMac(mac);
	}

	@Override
	public SensorDriver findSensorDriverByMac(String mac) {
		return getDao().findSensorDriverByMac(mac);
	}

	@Override
	public List<DeviceDto> findDtoByUserZoneId(User user, long zoneId) {
		List<Device> devices = deviceDao.findByUserZoneId(user.getId(), zoneId);		
		return deviceMapper.toDeviceDtoList(devices, playerService);
	}

	@Override
	public MediaPlayer getPlayer(Long playerId) {
		if (playerId != null) {
			Device device = findById(playerId);
			Player player = (net.ionoff.center.server.entity.Player) 
					EntityUtil.castUnproxy(device, Device.class);
			if (player != null) {
				return MediaPlayer.fromPlayer(device);
			}
		}
		return null;
	}

	@Override
	public DeviceDto insertDto(User user, DeviceDto dto) {
		Device device = deviceMapper.createDevice(dto, zoneDao.findById(dto.getZoneId()));
		insert(device);
		return deviceMapper.createDeviceDto(device, playerService);
	}

	@Override
	public DeviceDto updateDto(User user, DeviceDto dto) {
		Device device = requireById(dto.getId());
		deviceMapper.updateDevice(device, dto);
		update(device, dto.getZoneId());
		return deviceMapper.createDeviceDto(device, playerService);
	}

	@Override
	public DeviceDto requireDtoById(long id) {
		Device device = requireById(id);
		return deviceMapper.createDeviceDto(device, playerService);
	}
	
	@Override
	public void deleteDtoById(User user, long entityId) {
		super.deleteById(entityId);
		Cache cache = getDao().getSessionFactory().getCache();
		if (cache != null) {
		    cache.evictAllRegions();
		}
	}

	@Override
	public List<StatusDto> getStatusByZoneId(User user, Long zoneId) {
		List<Device> devices = getDao().findByUserZoneId(user.getId(), zoneId);
		return deviceMapper.toStatusDto(devices, playerService);
	}

	@Override
	public List<StatusDto> getStatusByProjectId(User user, Long projectId) {
		List<Device> devices = getDao().findByUserProjectId(user.getId(), projectId);
		return deviceMapper.toStatusDto(devices, playerService);
	}

	@Override
	public List<DeviceDto> findDtoByUserProjectId(User user, long projectId) {
		List<Device> devices = deviceDao.findByUserProjectId(user.getId(), projectId);
		return deviceMapper.toDeviceDtoList(devices, playerService);
	}

	@Override
	protected List<DeviceDto> createDtoList(List<Device> devices) {
		return deviceMapper.toDeviceDtoList(devices, playerService);
	}

	@Override
	public void updateSensorStatus(Sensor sensor) {
		sensorService.update(sensor);
		sensorService.updateStatus(sensor.getStatus());
	}

	@Override
	@Async
	public void onSensorStatusChanged(Sensor sensor) {
		sensorService.insertSensorData(sensor.getStatus());
		sensorStatusNotifier.notifyListeners(new SensorStatusChangedEvent(sensor));
	}
	
}
