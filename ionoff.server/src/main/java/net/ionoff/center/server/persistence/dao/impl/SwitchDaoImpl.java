package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.Switch;
import net.ionoff.center.server.persistence.dao.ISwitchDao;

@Transactional
public class SwitchDaoImpl extends AbstractGenericDao<Switch> implements ISwitchDao {
	
	public SwitchDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Switch.class);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<Switch> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}
}
