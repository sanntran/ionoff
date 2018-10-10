package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.UserGroup;
import net.ionoff.center.server.persistence.dao.IUserGroupDao;

@Repository
@Transactional
public class UserGroupDaoImpl extends AbstractGenericDao<UserGroup> implements IUserGroupDao {

	@Autowired
	protected UserGroupDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(UserGroup.class);
	}

	@Override
	public UserGroup findByName(String name) {
		String sql = "select distinct group_"
				+ " from UserGroup as group_" 
				+ " where group_.name = :name";
	
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("name", name);
	
		List<UserGroup> groups = findMany(query);
		
		return getFirst(groups);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<UserGroup> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}
}
