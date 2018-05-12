package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeSensor;

@Transactional
public interface IModeSensorDao extends IGenericDao<ModeSensor> {

	List<ModeSensor> findBySensorId(Long sensorId);
	
}
