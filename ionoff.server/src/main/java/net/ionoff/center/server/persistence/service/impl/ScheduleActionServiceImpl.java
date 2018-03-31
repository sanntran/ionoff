package net.ionoff.center.server.persistence.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ScheduleAction;
import net.ionoff.center.server.entity.SchedulePlayerAction;
import net.ionoff.center.server.entity.ScheduleRelayAction;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.objmapper.ScheduleMapper;
import net.ionoff.center.server.persistence.dao.IScheduleActionDao;
import net.ionoff.center.server.persistence.service.IScheduleActionService;
import net.ionoff.center.shared.dto.ScheduleActionDto;
import net.ionoff.center.shared.dto.SchedulePlayerActionDto;
import net.ionoff.center.shared.dto.ScheduleRelayActionDto;

@Transactional
public class ScheduleActionServiceImpl extends AbstractGenericService<ScheduleAction, ScheduleActionDto> implements IScheduleActionService {

	private IScheduleActionDao scheduleActionDao;
	
	@Autowired
	private ScheduleMapper scheduleMapper;
	
	public ScheduleActionServiceImpl(IScheduleActionDao scheduleActionDao) {
		this.scheduleActionDao = scheduleActionDao;
	}

	@Override
	protected IScheduleActionDao getDao() {
		return scheduleActionDao;
	}

	@Override
	public ScheduleActionDto insertDto(User user, ScheduleActionDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public ScheduleActionDto updateDto(User user, ScheduleActionDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void deleteDtoById(User user, long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public ScheduleActionDto requireDtoById(long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public SchedulePlayerActionDto updateSchedulePlayerActionDto(SchedulePlayerActionDto schedulePlayerActionDto) {
		
		final SchedulePlayerAction scheduleAction = (SchedulePlayerAction) requireById(schedulePlayerActionDto.getId());

		scheduleAction.setAction(schedulePlayerActionDto.getAction());
		scheduleAction.setVolume(schedulePlayerActionDto.getVolume());
		scheduleAction.setAlbum(schedulePlayerActionDto.getAlbum());
		scheduleAction.setAlbumType(schedulePlayerActionDto.getAlbumType());
		
		update(scheduleAction);
		return (SchedulePlayerActionDto) scheduleMapper.createScheduleActionDto(scheduleAction);
	}

	@Override
	public ScheduleRelayActionDto updateScheduleRelayActionDto(ScheduleRelayActionDto scheduleRelayActionDto) {
		
		final ScheduleRelayAction scheduleAction = (ScheduleRelayAction) requireById(scheduleRelayActionDto.getId());
		scheduleAction.setAction(scheduleRelayActionDto.getAction());
		
		update(scheduleAction);

		return (ScheduleRelayActionDto) scheduleMapper.createScheduleActionDto(scheduleAction);
	}

	@Override
	protected List<ScheduleActionDto> createDtoList(List<ScheduleAction> entities) {
		throw new UnsupportedOperationException();
	}
}
