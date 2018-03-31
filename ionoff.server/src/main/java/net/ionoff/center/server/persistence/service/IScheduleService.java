package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Schedule;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.shared.dto.ScheduleDto;

@Transactional
public interface IScheduleService extends IGenericService<Schedule, ScheduleDto> {
	
	List<Schedule> findByProjectId(long projectId);
	
	List<Schedule> findFailedSchedules(long projectId);
	
	List<Schedule> findEnabledSchedules(long projectId, String scheduleTime);
	
	List<ScheduleDto> findDtoByZoneId(long zoneId);

	List<ScheduleDto> findDtoByProjectId(long projectId);

	List<ScheduleDto> findDtoByUserZone(User user, Long zoneId);
	
	List<ScheduleDto> findDtoByUserProject(User user, Long projectId);
}
