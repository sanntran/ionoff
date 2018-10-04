package net.ionoff.center.server.persistence.dao.impl;

import java.util.Collections;
import java.util.List;

import net.ionoff.center.server.entity.PlayList;
import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.IPlayListDao;

@Transactional
public class PlayListDaoImpl extends AbstractGenericDao<PlayList> implements IPlayListDao {

	public PlayListDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(PlayList.class);
	}

	@Override
	public List<PlayList> findByUserId(long userId) {
		String sql = "select distinct playList"
				+ " from PlayList as playList"
				+ " where playList.user.id = :userId"
				+ " order by playList.name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("userId", userId);
		
		return findMany(query);
	}

	@Override
	public List<PlayList> findByUserName(String userName) {
		String sql = "select distinct playList"
				+ " from PlayList as playList"
				+ " where playList.user.name = :userName"
				+ " order by playList.name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("userName", userName);
		
		return findMany(query);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<PlayList> findByCriteria(QueryCriteria criteria) {
		//throw new UnsupportedOperationException();
		return Collections.emptyList();
	}

}
