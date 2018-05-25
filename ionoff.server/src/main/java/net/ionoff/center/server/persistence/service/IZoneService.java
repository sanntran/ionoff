package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.shared.dto.ZoneDto;

@Transactional
public interface IZoneService extends IGenericService<Zone, ZoneDto> {
	
	List<Zone> findByProjectId(long projectId);
	
	List<Zone> findByAreaId(long areaId);

	void update(Zone zone, Long areaId);

	List<ZoneDto> findDtoByUserProjectId(long userId, long projectId);

}
