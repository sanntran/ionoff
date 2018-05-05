package net.ionoff.center.server.persistence.dao;

import net.ionoff.center.server.entity.QueryCriteria;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.SensorData;

import java.util.List;

@Transactional
public interface ISensorDataDao extends IGenericDao<SensorData> {

    long countByDay(QueryCriteria criteria);

    List<SensorData> getSumByDay(QueryCriteria criteria);

    List<SensorData> findByDay(QueryCriteria criteria);
}
