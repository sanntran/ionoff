package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.UserProject;
import net.ionoff.center.server.persistence.dao.IUserProjectDao;

@Repository
@Transactional
public class UserProjectDaoImpl extends AbstractGenericDao<UserProject> implements IUserProjectDao {

	@Autowired
	public UserProjectDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(UserProject.class);
	}

	@Override
	public List<UserProject> findByUserId(Long userId) {
		String sql = "select distinct userProject"
				+ " from UserProject as userProject"
				+ " where userProject.user.id = :userId"
				+ " order by userProject.id";
	
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("userId", userId);
		
		return findMany(query);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<UserProject> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}
}
