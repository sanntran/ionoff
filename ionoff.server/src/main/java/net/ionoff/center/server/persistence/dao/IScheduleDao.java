package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Schedule;

@Transactional
public interface IScheduleDao extends IGenericDao<Schedule> {

	List<Schedule> findByZoneId(long zoneId);
	
	List<Schedule> findByProjectId(long projectId);
	
	List<Schedule> findByDeviceId(long deviceId);

	List<Schedule> findFailedSchedules(long projectId);

	List<Schedule> findEnabledSchedules(long projectId, String timeHHMMAmPm);

	List<Schedule> findByUserProjectId(long userId, long projectId);

	List<Schedule> findByUserZoneId(long id, Long zoneId);
	
}
