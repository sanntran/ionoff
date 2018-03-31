package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Area;
import net.ionoff.center.shared.dto.AreaDto;

@Transactional
public interface IAreaService extends IGenericService<Area, AreaDto> {
	
	List<Area> findByProjectId(long projectId);
	
	List<AreaDto> findDtoByProjectId(long projectId);
	
	List<AreaDto> findByProjectId(Long projectId, Boolean includingZone, Boolean includingDevice);
}
