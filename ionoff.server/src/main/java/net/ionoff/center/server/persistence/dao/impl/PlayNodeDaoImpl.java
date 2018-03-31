package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.IPlayNodeDao;
import net.xapxinh.center.server.entity.PlayNode;

@Transactional
public class PlayNodeDaoImpl extends AbstractGenericDao<PlayNode> implements IPlayNodeDao {

	public PlayNodeDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(PlayNode.class);
	}

	@Override
	public List<PlayNode> findByPlayListId(long playListId) {
		String sql = "select distinct node"
				+ " from PlayNode as node"
				+ " where node.playList.id = :playListId"
				+ " order by node.idx";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("playListId", playListId);
		
		return findMany(query);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<PlayNode> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}
}
