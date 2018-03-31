package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.RelayGroup;
import net.ionoff.center.server.persistence.dao.IRelayGroupDao;

@Transactional
public class RelayGroupDaoImpl extends AbstractGenericDao<RelayGroup> implements IRelayGroupDao {

	public RelayGroupDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(RelayGroup.class);
	}

	@Override
	public synchronized RelayGroup update(RelayGroup relayGroup) {
		return super.update(relayGroup);
		
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<RelayGroup> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}
	
}
