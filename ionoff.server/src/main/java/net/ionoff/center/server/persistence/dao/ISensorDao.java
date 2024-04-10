package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Sensor;

@Transactional
public interface ISensorDao extends IGenericDao<Sensor> {

	List<Sensor> findByDeviceId(long deviceId);
	
	List<Sensor> findByProjectId(long projectId);

    List<Sensor> findOfflineInProject(long projectId);

    List<Sensor> findHavingAlertInProject(long projectId);
}
