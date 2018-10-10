package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.IPlayLeafDao;
import net.ionoff.center.server.entity.PlayLeaf;

@Repository
@Transactional
public class PlayLeafDaoImpl extends AbstractGenericDao<PlayLeaf> implements IPlayLeafDao {

	@Autowired
	public PlayLeafDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(PlayLeaf.class);
	}

	@Override
	public List<PlayLeaf> findByNodeId(long playNodeId) {
		String sql = "select distinct leaf"
				+ " from PlayLeaf as leaf"
				+ " where leaf.playNode.id = :playNodeId"
				+ " order by leaf.idx";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("playNodeId", playNodeId);
		
		return findMany(query);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<PlayLeaf> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}
}
