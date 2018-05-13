package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Mode;
import net.ionoff.center.shared.dto.ModeDto;

@Transactional
public interface IModeService extends IGenericService<Mode, ModeDto> {
	
	List<Mode> findByProjectId(long projectId);

	List<Mode> findByScheduleTime(String scheduleTime);
	
	List<ModeDto> findDtoByProjectId(long projectId);

	ModeDto findActivatedDtoByProjectId(long projectId);
	
}
