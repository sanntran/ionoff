package net.ionoff.center.server.persistence.dao;

import java.util.List;

import net.ionoff.center.server.entity.Sensor;

import org.springframework.transaction.annotation.Transactional;

@Transactional
public interface ISensorDao extends IGenericDao<Sensor> {
	
	List<Sensor> findByProjectId(long projectId);

	List<Sensor> findByControllerId(long controllerId);
	
}
