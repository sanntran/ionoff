package net.ionoff.center.server.objmapper;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.BaseObj;
import net.ionoff.center.server.entity.Schedule;
import net.ionoff.center.server.entity.ScheduleAction;
import net.ionoff.center.server.entity.SchedulePlayerAction;
import net.ionoff.center.server.entity.ScheduleRelayAction;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.persistence.service.IProjectService;
import net.ionoff.center.server.persistence.service.IScheduleService;
import net.ionoff.center.shared.dto.ScheduleActionDto;
import net.ionoff.center.shared.dto.ScheduleDto;
import net.ionoff.center.shared.dto.SchedulePlayerActionDto;
import net.ionoff.center.shared.dto.ScheduleRelayActionDto;

public class ScheduleMapper {
	
	@Autowired
	private IProjectService projectService;
	@Autowired
	private IDeviceService deviceService;
	@Autowired
	private IScheduleService scheduleService;
	
	public List<ScheduleDto> createScheduleDtoList(List<Schedule> schedules) {
		final List<ScheduleDto> scheduleDtos = new ArrayList<ScheduleDto>();
		for (final Schedule schedule : schedules) {
			scheduleDtos.add(createScheduleDto(schedule));
		}
		return scheduleDtos;
	}
	
	public Schedule createSchedule(ScheduleDto scheduleDto) {
		Schedule schedule = new Schedule();
		schedule.setDevice(deviceService.findById(scheduleDto.getDeviceId()));
		schedule.setProject(projectService.findById(scheduleDto.getProjectId()));
		updateSchedule(schedule, scheduleDto);
		return schedule;
	}

	public Schedule updateSchedule(Schedule schedule, ScheduleDto scheduleDto) {
		schedule.setName(scheduleDto.getName());
		schedule.setEnabled(scheduleDto.getEnabled());
		schedule.setRepeat(scheduleDto.getRepeat());
		schedule.setTime(scheduleDto.getTime());
		schedule.setDay(scheduleDto.getDay());
		return schedule;
	}
	
	public Schedule toSchedule(ScheduleDto scheduleDto) {
		final Schedule schedule = getOrCreateSchedule(scheduleDto.getId());
		schedule.setName(scheduleDto.getName());
		schedule.setEnabled(scheduleDto.getEnabled());
		schedule.setRepeat(scheduleDto.getRepeat());
		schedule.setTime(scheduleDto.getTime());
		schedule.setDay(scheduleDto.getDay());
		schedule.setDevice(deviceService.findById(scheduleDto.getDeviceId()));
		schedule.setProject(projectService.findById(scheduleDto.getProjectId()));
		return schedule;
	}
	
	public Schedule getOrCreateSchedule(long scheduleId) {
		if (BaseObj.isNew(scheduleId)) {
			final Schedule schedule = new Schedule();
			return schedule;
		}
		else {
			final Schedule schedule = scheduleService.findById(scheduleId);
			return schedule;
		}
	}
	

	public ScheduleDto createScheduleDto(Schedule schedule) {
		final ScheduleDto scheduleDto = new ScheduleDto();
		scheduleDto.setId(schedule.getId());
		scheduleDto.setName(schedule.getName());
		scheduleDto.setEnabled(schedule.getEnabled());
		scheduleDto.setRepeat(schedule.getRepeat());
		scheduleDto.setDay(schedule.getDay());
		scheduleDto.setTime(schedule.getTime());
		scheduleDto.setDeviceId(schedule.getDevice().getId());
		scheduleDto.setDeviceName(schedule.getDevice().getName());
		scheduleDto.setProjectId(schedule.getProject().getId());

		if (schedule.getActions() != null) {
			final List<ScheduleActionDto> scheduleActionDtos = new ArrayList<ScheduleActionDto>();
			for (final ScheduleAction scheduleAction : schedule.getActions()) {
				scheduleActionDtos.add(createScheduleActionDto(scheduleAction));
			}
			Collections.sort(scheduleActionDtos);
			scheduleDto.setActions(scheduleActionDtos);
		}
		return scheduleDto;
	}

	public ScheduleActionDto createScheduleActionDto(ScheduleAction scheduleAction) {
		final ScheduleActionDto scheduleActionDto = newScheduleActionDto(scheduleAction);

		scheduleActionDto.setId(scheduleAction.getId());
		scheduleActionDto.setAction(scheduleAction.getAction());
		scheduleActionDto.setScheduleId(scheduleAction.getSchedule().getId());
		scheduleActionDto.setScheduleName(scheduleAction.getSchedule().getName());

		return scheduleActionDto;
	}
	
	private ScheduleActionDto newScheduleActionDto(ScheduleAction scheduleAction) {

		if (scheduleAction instanceof ScheduleRelayAction) {
			final ScheduleRelayAction scheduleRelayAction = (ScheduleRelayAction)scheduleAction;

			final ScheduleRelayActionDto scheduleRelayActionDto = new ScheduleRelayActionDto();
			scheduleRelayActionDto.setRelayId(scheduleRelayAction.getRelay().getId());
			scheduleRelayActionDto.setRelayName(scheduleRelayAction.getRelay().getName());
			scheduleRelayActionDto.setRelayType(scheduleRelayAction.getRelay().getType());

			return scheduleRelayActionDto;
		}
		else if (scheduleAction instanceof SchedulePlayerAction) {
			final SchedulePlayerAction schedulePlayerAction = (SchedulePlayerAction)scheduleAction;

			final SchedulePlayerActionDto schedulePlayerActionDto = new SchedulePlayerActionDto();
			schedulePlayerActionDto.setAlbum(schedulePlayerAction.getAlbum());
			schedulePlayerActionDto.setAlbumType(schedulePlayerAction.getAlbumType());
			schedulePlayerActionDto.setVolume(schedulePlayerAction.getVolume());
			schedulePlayerActionDto.setPlayerId(schedulePlayerAction.getSchedule().getDevice().getId());
			schedulePlayerActionDto.setPlayerName(schedulePlayerAction.getSchedule().getDevice().getName());

			return schedulePlayerActionDto;
		}

		return new ScheduleActionDto();
	}

}
