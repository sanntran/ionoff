package net.ionoff.center.server.persistence.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.hibernate.Cache;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Appliance;
import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.EntityUtil;
import net.ionoff.center.server.entity.Player;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.Scene;
import net.ionoff.center.server.entity.SceneDevice;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.entity.UserDevice;
import net.ionoff.center.server.entity.WeighScale;
import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.server.objmapper.DeviceMapper;
import net.ionoff.center.server.persistence.dao.IDeviceDao;
import net.ionoff.center.server.persistence.dao.IUserDao;
import net.ionoff.center.server.persistence.dao.IUserDeviceDao;
import net.ionoff.center.server.persistence.dao.IZoneDao;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.persistence.service.ISceneDeviceService;
import net.ionoff.center.server.util.DateTimeUtil;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.StatusDto;
import net.xapxinh.center.server.exception.UnknownPlayerException;
import net.xapxinh.center.server.service.player.IPlayerService;
import net.xapxinh.center.shared.dto.Status;

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
	protected IPlayerService playerService;
	
	@Autowired
	private DeviceMapper deviceMapper;
	
	@Autowired
	private ISceneDeviceService sceneDeviceService;
	
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
	public WeighScale findWeighScaleByMac(String mac) {
		return getDao().findWeighScaleByMac(mac);
	}

	@Override
	public List<DeviceDto> findDtoByUserZoneId(User user, long zoneId) {
		List<Device> devices = deviceDao.findByZoneId(user.getId(), zoneId);		
		return deviceMapper.toDeviceDtoList(devices);
	}

	public List<StatusDto> toStatusDto(List<Device> devices) {
		List<StatusDto> statusDtos = new ArrayList<>();
		for (Device device : devices) {
			statusDtos.add(getStatusDto(device));
		}
		return statusDtos;
	}
	
	@Override
	public StatusDto getStatusDto(Device device) {
		StatusDto statusDto = new StatusDto();
		statusDto.setId(device.getId());
		statusDto.setValue(device.getStatus());
		if (device.getTime() != null) {
			statusDto.setTime(DateTimeUtil.ddMMHHmmFormatter.format(device.getTime()));
		}
		if (device.instanceOf(Appliance.class)) {
			for (Relay relay : device.getRelayList()) {
				StatusDto child = new StatusDto();
				child.setId(relay.getId());
				child.setValue(relay.getStatus());
				if (relay.getTime() != null) {
					child.setTime(DateTimeUtil.ddMMHHmmFormatter.format(relay.getTime()));
				}
				statusDto.getChildren().add(child);
			}
		}
		else if (device.instanceOf(Player.class)) {
			try {
				Status status = playerService.requesStatus(getPlayer(device.getId()), null);
				statusDto.setState(status.getState());
				statusDto.setTrack(status.getTitle());
				if (status.getPosition() > 0) {
					statusDto.setPosition(Math.round(status.getPosition() * 100));
				}
			} catch (Exception e) {
				//
			}
		}
		return statusDto;
	}

	@Override
	public net.xapxinh.center.server.entity.Player getPlayer(Long playerId) throws UnknownPlayerException {
		if (playerId != null) {
			Device device = findById(playerId);
			Player player = (net.ionoff.center.server.entity.Player) 
					EntityUtil.castUnproxy(device, Device.class);
			if (player != null) {
				net.xapxinh.center.server.entity.Player p = new net.xapxinh.center.server.entity.Player();
				p.setId(player.getId());
				p.setName(player.getName());
				p.setMac(player.getMac());
				p.setIp(player.getIp());
				p.setPort(player.getPort());
				p.setModel(player.getModel());
				return p;
			}
		}
		throw new UnknownPlayerException(playerId + "");
	}

	@Override
	public DeviceDto insertDto(User user, DeviceDto dto) {
		Device device = deviceMapper.createDevice(dto);
		insert(device);
		return deviceMapper.createDeviceDto(device);
	}

	@Override
	public DeviceDto updateDto(User user, DeviceDto dto) {
		Device device = requireById(dto.getId());
		deviceMapper.updateDevice(device, dto);
		update(device, dto.getZoneId());
		return deviceMapper.createDeviceDto(device);
	}

	@Override
	public DeviceDto requireDtoById(long id) {
		Device device = requireById(id);
		return deviceMapper.createDeviceDto(device);
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
		List<Device> devices = getDao().findByZoneId(user.getId(), zoneId);
		return toStatusDto(devices);
	}

	@Override
	public List<StatusDto> getStatusByProjectId(User user, Long projectId) {
		List<Device> devices = getDao().findByUserProjectId(user.getId(), projectId);
		return toStatusDto(devices);
	}

	@Override
	public List<DeviceDto> findDtoByUserProjectId(User user, long projectId) {
		List<Device> devices = deviceDao.findByUserProjectId(user.getId(), projectId);
		return deviceMapper.toDeviceDtoList(devices);
	}

	@Override
	protected List<DeviceDto> createDtoList(List<Device> entities) {
		return deviceMapper.toDeviceDtoList(entities);
	}
	
}
