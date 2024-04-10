package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Zone;

@Transactional
public interface IZoneDao extends IGenericDao<Zone> {
	
	List<Zone> findByProjectId(long projectId);
	
	List<Zone> findByAreaId(long areaId);

	List<Zone> findByUserProjectId(long userId, long projectId);

    List<Zone> findHavingAlertInProject(long projectId);
}
