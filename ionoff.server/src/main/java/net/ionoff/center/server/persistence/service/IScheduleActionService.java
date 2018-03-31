package net.ionoff.center.server.persistence.service;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ScheduleAction;
import net.ionoff.center.shared.dto.ScheduleActionDto;
import net.ionoff.center.shared.dto.SchedulePlayerActionDto;
import net.ionoff.center.shared.dto.ScheduleRelayActionDto;

@Transactional
public interface IScheduleActionService extends IGenericService<ScheduleAction, ScheduleActionDto> {

	SchedulePlayerActionDto updateSchedulePlayerActionDto(SchedulePlayerActionDto schedulePlayerActionDto);

	ScheduleRelayActionDto updateScheduleRelayActionDto(ScheduleRelayActionDto scheduleRelayActionDto);
	
}
