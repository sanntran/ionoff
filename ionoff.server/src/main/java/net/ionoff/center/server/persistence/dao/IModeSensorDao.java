package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.server.entity.Sensor;

@Transactional
public interface IModeSensorDao extends IGenericDao<ModeSensor> {

	List<ModeSensor> findBySensorId(Long sensorId);

	List<ModeSensor> findOnSensorStatusChanged(Sensor sensor);
	
}
