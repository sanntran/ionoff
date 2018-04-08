package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.SensorData;
import net.ionoff.center.server.persistence.dao.ISensorDataDao;

@Transactional
public class SensorDataDaoImpl extends AbstractGenericDao<SensorData> implements ISensorDataDao {
	
	public SensorDataDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(SensorData.class);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<SensorData> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}
}
