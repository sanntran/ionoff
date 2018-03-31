package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.IPlayLeafDao;
import net.xapxinh.center.server.entity.PlayLeaf;

@Transactional
public class PlayLeafDaoImpl extends AbstractGenericDao<PlayLeaf> implements IPlayLeafDao {

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
