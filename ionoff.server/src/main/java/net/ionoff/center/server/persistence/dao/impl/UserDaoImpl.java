package net.ionoff.center.server.persistence.dao.impl;

import java.util.Collections;
import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.persistence.dao.IUserDao;

@Transactional
public class UserDaoImpl extends AbstractGenericDao<User> implements IUserDao {

	public UserDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(User.class);
	}

	@Override
	public User findByName(String userName) {
		String sql = "select distinct user_"
					+ " from User as user_" 
					+ " where user_.name = :userName";
		
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("userName", userName);

		List<User> users = findMany(query);
		
		return getFirst(users);
	}

	@Override
	public List<User> findByGroupId(int groupId) {
		String sql = "select distinct user_"
					+ " from User as user_, UserGroup as group_" 
					+ " where user_.groupId = :groupId";
					
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("groupId", groupId);

		return findMany(query);
	}

	private long countByName(long projectId, String name) {
		if (name == null || name.isEmpty()) {
			return countByProjectId(projectId);
			
		}
		String sql = "select count(user_)"
					+ " from User as user_, UserProject as userProject"
					+ " where user_.id = userProject.user.id"
					+ " and userProject.role = true"
					+ " and userProject.project.id = :projectId"
					+ " and lower(user_.name) like :name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("name", "%" + name.toLowerCase() + "%");
		return countObjects(query);
	}

	private List<User> findByProjectId(long projectId, int fromIndex, int maxResults, boolean isAscending) {
		String sql = "select distinct user_"
					+ " from User as user_, UserProject as userProject"
					+ " where user_.id = userProject.user.id"
					+ " and userProject.role = true"
					+ " and userProject.project.id = :projectId"
					+ " order by user_.name";
		if (!isAscending) {
			sql = sql + " desc";
		}
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("projectId", projectId);
		return findMany(query, fromIndex, maxResults);
	}

	@Override
	public List<User> findByProjectId(long projectId) {
		String sql = "select distinct user_"
				+ " from User as user_, UserProject as userProject"
				+ " where user_.id = userProject.user.id"
				+ " and userProject.role = true"
				+ " and userProject.project.id = :projectId"
				+ " order by user_.name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId);
		return findMany(query);
	}

	private long countByProjectId(long projectId) {
		String sql = "select count(user_)"
				+ " from User as user_, UserProject as userProject"
				+ " where user_.id = userProject.user.id"
				+ " and userProject.role = true"
				+ " and userProject.project.id = :projectId";
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("projectId", projectId);
		return countObjects(query);
	}

	private List<User> findByName(long projectId, String name, int fromIndex, int maxResults, 
			String sortBy, boolean isAscending) {
		if (name == null || name.isEmpty()) {
			return findByProjectId(projectId, fromIndex, maxResults, isAscending);
		}
		String sql = "select distinct user_" 
					+ " from User as user_, UserProject as userProject" 
					+ " where user_.id = userProject.user.id"
					+ " and userProject.project.id = :projectId"
					+ " and userProject.role = true"
					+ " and lower(user_.name) like :name" 
					+ " order by user_." + sortBy;
		if (!isAscending) {
			sql = sql + " desc";
		}
		
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("name", "%" + name.toLowerCase() + "%");
		return findMany(query, fromIndex, maxResults);
	}

	private Long countByName(String name) {
		if (name == null || name.isEmpty()) {
			return countAll();
		}
		String sql = "select count(user_)"
					+ " from User as user_" 
					+ " where lower(user_.name) like :name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("name", "%" + name.toLowerCase() + "%");
		return countObjects(query);
	}

	private Long countAll() {
		String sql = "select count(user_)"
				+ " from User as user_";
		Query query = getCurrentSession().createQuery(sql);
		return countObjects(query);
	}

	private List<User> findByName(String name, int fromIndex, int maxResults, String sortBy, boolean isAscending) {
		if (name == null || name.isEmpty()) {
			return getByRange(fromIndex, maxResults, isAscending);
		}
		String sql = "select distinct user_" 
					+ " from User as user_" 
					+ " where lower(user_.name) like :name" 
					+ " order by user_." + sortBy;
		if (!isAscending) {
			sql = sql + " desc";
		}
		
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("name", "%" + name.toLowerCase() + "%");
		return findMany(query, fromIndex, maxResults);
	}

	private List<User> getByRange(int fromIndex, int maxResults, boolean isAscending) {
			String sql = "select distinct user_"
					+ " from User as user_"
					+ " order by user_.name";
		if (!isAscending) {
			sql = sql + " desc";
		}
		Query query = getCurrentSession().createQuery(sql);
		return findMany(query, fromIndex, maxResults);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		if (criteria.getProjectId() == null) {
			if (NAME.equals(criteria.getSearchField())) {
				return countByName(criteria.getSearchKey());
			}
			return 0L;
		}
		else {
			if (NAME.equals(criteria.getSearchField())) {
				return countByName(criteria.getProjectId(), criteria.getSearchKey());
			}
			return 0;
		}
	}

	@Override
	public List<User> findByCriteria(QueryCriteria criteria) {
		if (criteria.getProjectId() == null) {
			if (NAME.equals(criteria.getSearchField())) {
				return findByName(criteria.getSearchKey(), 
						criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
			}
			return Collections.emptyList();
		}
		else {
			if (NAME.equals(criteria.getSearchField())) {
				return findByName(criteria.getProjectId(), criteria.getSearchKey(), 
						criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
			}
			return Collections.emptyList();
		}
	}
}
