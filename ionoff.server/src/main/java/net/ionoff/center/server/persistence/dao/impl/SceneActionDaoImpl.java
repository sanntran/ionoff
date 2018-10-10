package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.SceneAction;
import net.ionoff.center.server.persistence.dao.ISceneActionDao;

@Repository
@Transactional
public class SceneActionDaoImpl extends AbstractGenericDao<SceneAction> implements ISceneActionDao {

	@Autowired
	public SceneActionDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(SceneAction.class);
	}

	@Override
	public void deleteByRelayId(long relayId) {
		 String sql = "delete from SceneAction where relay.id = :relayId";
		 Query query = getCurrentSession().createQuery(sql)
					.setParameter("relayId", relayId);
		 query.executeUpdate();
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<SceneAction> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}
}
