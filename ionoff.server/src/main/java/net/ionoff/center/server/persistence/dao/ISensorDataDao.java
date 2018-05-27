package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.SensorData;

@Transactional
public interface ISensorDataDao extends IGenericDao<SensorData> {

    long countByDay(QueryCriteria criteria);

    List<SensorData> findByDay(QueryCriteria criteria);
}
