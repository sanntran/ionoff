package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Mode;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.IModeDao;

@Transactional
public class ModeDaoImpl extends AbstractGenericDao<Mode> implements IModeDao {
	
	public ModeDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Mode.class);
	}

	private long countByProjectId(long projectId) {
		String sql = "select count(mode)"
				+ " from Mode as mode"
				+ " where mode.project.id = :projectId";
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("projectId", projectId);
		return countObjects(query);
	}

	private long countByProjectIdName(long projectId, String name) {
		String sql = "select count(mode)"
				+ " from Mode as mode"
				+ " where mode.project.id = :projectId"
				+ " and lower(mode.name) like :name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("name", "%" + name.toLowerCase() + "%");
		return countObjects(query);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		if (criteria.isBlankKey()) {
			return countByProjectId(criteria.getProjectId());
		}
		if (NAME.equals(criteria.getSearchField())) {
			return countByProjectIdName(criteria.getProjectId(), criteria.getSearchKey());
		}
		return 0;
	}

	@Override
	public List<Mode> findByCriteria(QueryCriteria criteria) {
		String sql = "select distinct mode" 
				+ " from Mode as mode" 
				+ " where mode.project.id = :projectId";
				
		if (!criteria.isBlankKey()) {
			sql = sql + " and mode.name like :name";
		}
		
		sql = sql + " order by mode." + criteria.getSortBy();
		if (!criteria.getIsAscending()) {
			sql = sql + " desc";
		}
		
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", criteria.getProjectId());
		if (!criteria.isBlankKey()) {
			query.setParameter("name", "%" + criteria.getSearchKey() + "%");
		}
		return findMany(query, criteria.getFromIndex(), criteria.getMaxResults());
	}

	@Override
	public List<Mode> findByProjectId(long projectId) {
		
		String sql = "select distinct mode"
				+ " from Mode as mode"
				+ " where mode.project.id = :projectId"
				+ " order by mode.order, mode.name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId);
		
		return findMany(query);
	}
	
	private List<Mode> findByProjectId(long projectId, int fromIndex, int maxResults, boolean isAscending) {
		String sql = "select distinct mode"
					+ " from Mode as mode"
					+ " where mode.project.id = :projectId"
					+ " order by mode.order, mode.name";
		
		if (!isAscending) {
			sql = sql + " desc";
		}
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("projectId", projectId);
		return findMany(query, fromIndex, maxResults);
	}

	@Override
	public List<Mode> findByScheduleTime(String scheduleTime) {
		String sql = "select distinct mode"
				+ " from Mode as mode"
				+ " where mode.isScheduled = :isScheduled"
				+ " and mode.scheduleTime = :scheduleTime"
				+ " order by mode.project.id, mode.order";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("isScheduled", true)
				.setParameter("scheduleTime", scheduleTime);
		
		return findMany(query);
	}
}
