package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.Version;
import net.ionoff.center.server.persistence.dao.IVersionDao;

@Transactional
public class VersionDaoImpl extends AbstractGenericDao<Version> implements IVersionDao {

	public VersionDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Version.class);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<Version> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}
}
