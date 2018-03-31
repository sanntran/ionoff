package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.IModeSensorDao;

@Transactional
public class ModeSensorDaoImpl extends AbstractGenericDao<ModeSensor> implements IModeSensorDao {
	
	public ModeSensorDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(ModeSensor.class);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<ModeSensor> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}
}
