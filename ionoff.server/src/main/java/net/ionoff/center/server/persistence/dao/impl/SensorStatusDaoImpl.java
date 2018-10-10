package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.SensorStatus;
import net.ionoff.center.server.persistence.dao.ISensorStatusDao;

@Repository
@Transactional
public class SensorStatusDaoImpl extends AbstractGenericDao<SensorStatus> implements ISensorStatusDao {

	@Autowired
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
