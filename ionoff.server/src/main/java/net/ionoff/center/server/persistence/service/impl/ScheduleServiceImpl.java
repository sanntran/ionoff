package net.ionoff.center.server.persistence.service.impl;

import java.util.List;

import org.hibernate.Cache;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.Player;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.Schedule;
import net.ionoff.center.server.entity.SchedulePlayerAction;
import net.ionoff.center.server.entity.ScheduleRelayAction;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.UpdateEntityException;
import net.ionoff.center.server.locale.Constants;
import net.ionoff.center.server.locale.Messages;
import net.ionoff.center.server.objmapper.ScheduleMapper;
import net.ionoff.center.server.persistence.dao.IScheduleActionDao;
import net.ionoff.center.server.persistence.dao.IScheduleDao;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.persistence.service.IScheduleService;
import net.ionoff.center.shared.dto.ScheduleDto;

@Transactional
public class ScheduleServiceImpl extends AbstractGenericService<Schedule, ScheduleDto> implements IScheduleService {

	private IScheduleDao scheduleDao;
	
	@Autowired
	private IScheduleActionDao scheduleActionDao;
	
	@Autowired
	private ScheduleMapper scheduleMapper;
	
	@Autowired
	private IDeviceService deviceService;
	
	public ScheduleServiceImpl(IScheduleDao scheduleDao) {
		this.scheduleDao = scheduleDao;
	}

	@Override
	protected IScheduleDao getDao() {
		return scheduleDao;
	}

	@Override
	public Schedule insert(Schedule schedule) {
		super.insert(schedule);
		insertScheduleActions(schedule);
		Cache cache = scheduleDao.getSessionFactory().getCache();
		if (cache != null) {
		    cache.evictAllRegions();
		}
		return schedule;
	}
	
	private void insertScheduleActions(Schedule schedule) {
		Device device = schedule.getDevice();
		if (device instanceof Player) {
			insertSchedulePlayerAction(schedule);
		}
		else {
			insertScheduleRelaysAction(schedule);
		}
	}

	private void insertScheduleRelaysAction(Schedule schedule) {
		List<Relay> relays = schedule.getDevice().getRelayList();
		for (Relay relay : relays) {
			ScheduleRelayAction scheduleAction = new ScheduleRelayAction();
			scheduleAction.setAction(SchedulePlayerAction.NONE);
			scheduleAction.setSchedule(schedule);
			scheduleAction.setRelay(relay);
			scheduleActionDao.insert(scheduleAction);
		}
	}

	private void insertSchedulePlayerAction(Schedule schedule) {
		SchedulePlayerAction scheduleAction = new SchedulePlayerAction();
		scheduleAction.setAction(SchedulePlayerAction.NONE);
		scheduleAction.setSchedule(schedule);
		scheduleAction.setPlayer((Player)schedule.getDevice());
		scheduleActionDao.insert(scheduleAction);
	}

	@Override
	public List<Schedule> findByProjectId(long projectId) {
		return getDao().findByProjectId(projectId);
	}

	@Override
	public List<Schedule> findFailedSchedules() {
		return getDao().findFailedSchedules();
	}

	@Override
	public List<Schedule> findEnabledSchedules(String scheduleTime) {
		return getDao().findEnabledSchedules(scheduleTime);
	}
	
	@Override
	public List<ScheduleDto> findDtoByProjectId(long projectId) {
		List<Schedule> schedules = scheduleDao.findByProjectId(projectId);
		return scheduleMapper.createScheduleDtoList(schedules);
	}

	@Override
	public List<ScheduleDto> findDtoByZoneId(long zoneId) {
		List<Schedule> schedules = scheduleDao.findByZoneId(zoneId);
		return scheduleMapper.createScheduleDtoList(schedules);
	}

	@Override
	public ScheduleDto requireDtoById(long id) {
		return scheduleMapper.createScheduleDto(requireById(id));
	}

	@Override
	public ScheduleDto insertDto(User user, ScheduleDto dto) {
		Schedule schedule = scheduleMapper.createSchedule(dto, deviceService);
		validateSchedule(schedule, user.getLanguage());
		insert(schedule);
		return scheduleMapper.createScheduleDto(schedule);
	}

	private void validateSchedule(Schedule schedule, String language) {
		if (schedule.getDevice() == null) {
			final String message = Messages.get(language).fieldInvalid(Constants.get(language).device(), "null");
			throw new UpdateEntityException(message);
		}
	}

	@Override
	public ScheduleDto updateDto(User user, ScheduleDto dto) {
		Schedule schedule = requireById(dto.getId());
		scheduleMapper.updateSchedule(schedule, dto);
		validateSchedule(schedule, user.getLanguage());
		update(schedule);
		Cache cache = scheduleDao.getSessionFactory().getCache();
		if (cache != null) {
		    cache.evictAllRegions();
		}
		return scheduleMapper.createScheduleDto(schedule);
	}

	@Override
	public void deleteDtoById(User user, long id) {
		deleteById(id);
		Cache cache = getDao().getSessionFactory().getCache();
		if (cache != null) {
		    cache.evictAllRegions();
		}
	}

	@Override
	public List<ScheduleDto> findDtoByUserProject(User user, Long projectId) {
		List<Schedule> schedules = getDao().findByUserProjectId(user.getId(), projectId); 
		return scheduleMapper.createScheduleDtoList(schedules);
	}

	@Override
	protected List<ScheduleDto> createDtoList(List<Schedule> entities) {
		return scheduleMapper.createScheduleDtoList(entities);
	}

	@Override
	public List<ScheduleDto> findDtoByUserZone(User user, Long zoneId) {
		List<Schedule> schedules = getDao().findByUserZoneId(user.getId(), zoneId); 
		return scheduleMapper.createScheduleDtoList(schedules);
	}
}
