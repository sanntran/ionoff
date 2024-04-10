package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Area;

@Transactional
public interface IAreaDao extends IGenericDao<Area> {
	
	List<Area> findByProjectId(long projectId);

    List<Area> findHavingAlertInProject(long projectId);
}
