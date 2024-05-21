package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeScene;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.IModeSceneDao;

@Repository
@Transactional
public class ModeSceneDaoImpl extends AbstractGenericDao<ModeScene> implements IModeSceneDao {

	public ModeSceneDaoImpl() {
		super();
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
