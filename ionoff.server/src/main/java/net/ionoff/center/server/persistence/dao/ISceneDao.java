package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Scene;

@Transactional
public interface ISceneDao extends IGenericDao<Scene> {


    long countByProjectId(long projectId);

    List<Scene> findByProjectId(long projectId);
	
	List<Scene> findByUserProjectId(long userId, Long projectId);
	
	List<Scene> findByUserZoneId(long userId, long zoneId);

    long countByZoneId(long zoneId);
}
