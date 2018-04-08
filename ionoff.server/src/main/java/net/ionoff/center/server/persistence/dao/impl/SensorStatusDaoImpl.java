package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.SensorStatus;
import net.ionoff.center.server.persistence.dao.ISensorStatusDao;

@Transactional
public class SensorStatusDaoImpl extends AbstractGenericDao<SensorStatus> implements ISensorStatusDao {
	
	public SensorStatusDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(SensorStatus.class);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<SensorStatus> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}
}
