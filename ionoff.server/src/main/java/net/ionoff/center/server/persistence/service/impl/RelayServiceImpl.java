package net.ionoff.center.server.persistence.service.impl;

import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.MediaPlayer;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.RelayLoad;
import net.ionoff.center.server.entity.SceneDevice;
import net.ionoff.center.server.entity.SceneRelayAction;
import net.ionoff.center.server.entity.Schedule;
import net.ionoff.center.server.entity.SchedulePlayerAction;
import net.ionoff.center.server.entity.ScheduleRelayAction;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.locale.Messages;
import net.ionoff.center.server.persistence.dao.IDeviceDao;
import net.ionoff.center.server.persistence.dao.IRelayDao;
import net.ionoff.center.server.persistence.dao.ISceneActionDao;
import net.ionoff.center.server.persistence.dao.ISceneDeviceDao;
import net.ionoff.center.server.persistence.dao.IScheduleActionDao;
import net.ionoff.center.server.persistence.dao.IScheduleDao;
import net.ionoff.center.server.persistence.mapper.RelayMapper;
import net.ionoff.center.server.persistence.service.IRelayService;
import net.ionoff.center.shared.dto.RelayDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;

@Service
@Transactional
public class RelayServiceImpl extends AbstractGenericService<Relay, RelayDto> implements IRelayService {
	
	private IRelayDao relayDao;
	
	@Autowired
	private ISceneActionDao sceneActionDao;
	
	@Autowired
	private ISceneDeviceDao sceneDeviceDao;
	@Autowired
	
	private IScheduleActionDao scheduleActionDao;
	
	@Autowired
	private IScheduleDao scheduleDao;
	
	@Autowired
	private IDeviceDao deviceDao;
	
	@Autowired
	private RelayMapper relayMapper;

	@Autowired
	public RelayServiceImpl(IRelayDao relayDao) {
		this.relayDao = relayDao;
	}

	@Override
	protected IRelayDao getDao() {
		return relayDao;
	}
	
	@Override
	public Relay update(Relay relay, boolean status) {
		synchronized (this) {
			relay.setStatus(status);
			relay.setTime(new Date());
			Relay r = findById(relay.getId());
			r.setStatus(status);
			r.setTime(new Date());
			Device device = r.getDevice();
			if (device != null) {
				device = deviceDao.findById(relay.getDevice().getId());
				if (device.hasOneRelay()) {
					device.setStatus(relay.getStatus());
				}
				device.setTime(relay.getTime());
				deviceDao.update(device);
			}
			return super.update(r);
		}
	}
	
	@Override
	public Relay update(Relay relay, Device newDevice) {
		Device oldDevice = relay.getDevice();
		if (!isDeviceChanged(oldDevice, newDevice)) {
			return update(relay);
		}
		deleteRelayActions(relay);
		relay.setDevice(newDevice);
		if (relay.getDevice() != null) {
			relay.setLabel(relay.getDevice().getNameId());
		}
		update(relay);
		insertRelayActions(relay);
		if (oldDevice != null) {
			oldDevice.getRelays().remove(relay);
		}
		if (newDevice != null) {
			newDevice.getRelays().add(relay);
		}
		deviceDao.updateDeviceStatus(oldDevice);
		deviceDao.updateDeviceStatus(newDevice);
		return relay;
	}

	private void insertRelayActions(Relay relay) {
		if (relay.getDevice() == null) {
			return;
		}
		insertSceneActions(relay);
		insertScheduleActions(relay);
	}

	private void insertScheduleActions(Relay relay) {
		List<Schedule> schedules = scheduleDao.findByDeviceId(relay.getDevice().getId());
		if (schedules == null) {
			return;
		}
		for (Schedule schedule : schedules) {
			insertScheduleAction(schedule, relay);
		}
	}

	private void insertScheduleAction(Schedule schedule, Relay relay) {
		ScheduleRelayAction scheduleAction = new ScheduleRelayAction();
		scheduleAction.setAction(SchedulePlayerAction.NONE);
		scheduleAction.setSchedule(schedule);
		scheduleAction.setRelay(relay);
		scheduleActionDao.insert(scheduleAction);
	}

	private void insertSceneActions(Relay relay) {
		List<SceneDevice> sceneDevices = sceneDeviceDao.findByDeviceId(relay.getDevice().getId());
		if (sceneDevices == null) {
			return;
		}
		for (SceneDevice sceneDevice : sceneDevices) {
			insertSceneAction(sceneDevice, relay);
		}
	}

	private void insertSceneAction(SceneDevice sceneDevice, Relay relay) {
		SceneRelayAction sceneAction = new SceneRelayAction();
		sceneAction.setRelay(relay);
		sceneAction.setSceneDevice(sceneDevice);
		sceneAction.setAction(SceneRelayAction.NONE);
		sceneActionDao.insert(sceneAction);
	}

	private void deleteRelayActions(Relay relay) {
		Device device = relay.getDevice();
		if (device == null) {
			return;
		}
		deleteSceneActions(relay);
		deleteScheduleActions(relay);
	}

	private void deleteScheduleActions(Relay relay) {
		scheduleActionDao.deleteByRelayId(relay.getId());
	}

	private void deleteSceneActions(Relay relay) {
		sceneActionDao.deleteByRelayId(relay.getId());
	}

	private boolean isDeviceChanged(Device fromDevice, Device toDevice) {
		if (fromDevice == null && toDevice == null) {
			return false;
		}
		if (fromDevice != null && toDevice != null 
				&& fromDevice.getId() == toDevice.getId()) {
			return false;
		}
		return true;
	}
	
	@Override
	public List<RelayDto> findDtoByProjectId(long projectId) {
		List<Relay> relays = getDao().findByProjectId(projectId);
		return relayMapper.createRelayDtoList(relays);
	}
	
	@Override
	public List<RelayDto> findDtoByRelayDriverId(long relayDriverId) {
		List<Relay> relays = getDao().findByRelayDriverId(relayDriverId);
		return relayMapper.createRelayDtoList(relays);
	}
	
	@Override
	public List<RelayDto> findDtoByDeviceId(long deviceId) {
		List<Relay> relays = getDao().findByDeviceId(deviceId);
		return relayMapper.createRelayDtoList(relays);
	}

	@Override
	public RelayDto insertDto(User user, RelayDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public RelayDto updateDto(User user, RelayDto dto) {
		Relay relay = requireById(dto.getId());
		relayMapper.updateRelay(relay, dto);
		if (dto.getDeviceId() == null) {
			update(relay, null);
			return relayMapper.createRelayDto(relay);
		}
		
		Device device = deviceDao.findById(dto.getDeviceId());
		if (device instanceof RelayLoad) {
			if (device.hasRelay() && !device.hasRelays(relay)) {
				throw new UpdateEntityException(Messages.get(user.getLanguage()).errorSetManyRelayForLight());
			}
		}
		if (device instanceof MediaPlayer) {
			throw new UpdateEntityException(Messages.get(user.getLanguage()).errorSetRelayForPlayer());
		}
		update(relay, device);
		return relayMapper.createRelayDto(relay);
	}

	@Override
	public RelayDto requireDtoById(long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void deleteDtoById(User user, long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected List<RelayDto> createDtoList(List<Relay> entities) {
		return relayMapper.createRelayDtoList(entities);
	}

}
