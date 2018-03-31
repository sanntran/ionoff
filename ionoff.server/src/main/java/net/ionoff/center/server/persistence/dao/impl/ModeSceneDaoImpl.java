package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeScene;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.IModeSceneDao;

@Transactional
public class ModeSceneDaoImpl extends AbstractGenericDao<ModeScene> implements IModeSceneDao {
	
	public ModeSceneDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(ModeScene.class);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<ModeScene> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}
}
