package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeSensorScene;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.IModeSensorSceneDao;

@Transactional
public class ModeSensorSceneDaoImpl extends AbstractGenericDao<ModeSensorScene> implements IModeSensorSceneDao {
	
	public ModeSensorSceneDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(ModeSensorScene.class);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<ModeSensorScene> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}
}
