package net.ionoff.center.server.persistence.dao;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.SensorData;

@Transactional
public interface ISensorDataDao extends IGenericDao<SensorData> {

}
