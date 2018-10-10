package net.ionoff.center.server.persistence.dao.impl;

import java.util.Collections;
import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.server.persistence.dao.IZoneDao;

@Repository
@Transactional
public class ZoneDaoImpl extends AbstractGenericDao<Zone> implements IZoneDao {

	@Autowired
	public ZoneDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Zone.class);
	}

	private long countByProjectId(long projectId) {
		String sql = "select count(zone)"
				+ " from Zone as zone"
				+ " where zone.project.id = :projectId";
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("projectId", projectId);
		return countObjects(query);
	}

	private long countByProjectIdName(long projectId, String name) {
		String sql = "select count(zone)"
				+ " from Zone as zone"
				+ " where zone.project.id = :projectId"
				+ " and lower(zone.name) like :name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("name", "%" + name.toLowerCase() + "%");
		return countObjects(query);
	}

	private long countByProjectIdAreaName(long projectId, String areaName) {
		String sql = "select count(zone)"
				+ " from Zone as zone"
				+ " where zone.project.id = :projectId"
				+ " and lower(zone.area.name) like :areaName";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("areaName", "%" + areaName.toLowerCase() + "%");
		return countObjects(query);
	}


	private List<Zone> findByProjectIdAreaName(long projectId, String keyWord,
			int fromIndex, int maxResults, String sortBy, boolean isAscending) {
		String sql = "select distinct zone"
				+ " from Zone as zone"
				+ " where zone.project.id = :projectId"
				+ " and zone.area.name like :keyWord"
				+ " order by zone.";

		if ("area".equals(sortBy)) {
		    sql = sql + sortBy + ", zone.order";
		}
		else {
            sql = sql + sortBy;
        }
		if (!isAscending) {
			sql = sql + " desc";
		}
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord + "%");
		return findMany(query, fromIndex, maxResults);
	}

	private List<Zone> findByProjectIdName(long projectId, String keyWord,
			int fromIndex, int maxResults, String sortBy, boolean isAscending) {

		String sql = "select distinct zone"
					+ " from Zone as zone"
					+ " where zone.project.id = :projectId"
					+ " and zone.name like :keyWord"
					+ " order by zone.";
		if ("area".equals(sortBy)) {
            sql = sql + sortBy + ", zone.order";
        }
		else {
		    sql = sql + sortBy;
		}
		if (!isAscending) {
			sql = sql + " desc";
		}
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord + "%");
		return findMany(query, fromIndex, maxResults);
	}

	@Override
	public List<Zone> findByProjectId(long projectId) {
		String sql = "select distinct zone"
				+ " from Zone as zone"
				+ " where zone.project.id = :projectId"
				+ " order by zone.area.order, zone.area.name, zone.order, zone.name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId);

		return findMany(query);
	}

	private List<Zone> findByProjectId(long projectId, int fromIndex, int maxResults,
			String sortBy, boolean isAscending) {
		String sql = "select distinct zone"
					+ " from Zone as zone"
					+ " where zone.project.id = :projectId"
					+ " order by zone.";
		if ("area".equals(sortBy)) {
            sql = sql + sortBy + ", zone.order";
        }
        else {
            sql = sql + sortBy;
        }
		if (!isAscending) {
			sql = sql + " desc";
		}
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("projectId", projectId);
		return findMany(query, fromIndex, maxResults);
	}

	@Override
	public List<Zone> findByAreaId(long areaId) {
		String sql = "select distinct zone"
				+ " from Zone as zone"
				+ " where zone.area.id = :areaId"
				+ " order by zone.order";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("areaId", areaId);

		return findMany(query);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		if (criteria.isBlankKey()) {
			return countByProjectId(criteria.getProjectId());
		}
		if (NAME.equals(criteria.getSearchField())) {
			return countByProjectIdName(criteria.getProjectId(), criteria.getSearchKey());
		}
		if (AREA_NAME.equals(criteria.getSearchField())) {
			return countByProjectIdAreaName(criteria.getProjectId(), criteria.getSearchKey());
		}
		return 0;
	}

	@Override
	public List<Zone> findByCriteria(QueryCriteria criteria) {
		
		if (criteria.isBlankKey()) {
			return findByProjectId(criteria.getProjectId(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
		}
		if (NAME.equals(criteria.getSearchField())) {
			return findByProjectIdName(criteria.getProjectId(), criteria.getSearchKey(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
		}
		if (AREA_NAME.equals(criteria.getSearchField())) {
			return findByProjectIdAreaName(criteria.getProjectId(), criteria.getSearchKey(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
		}

		return Collections.emptyList();
	}

	@Override
	public List<Zone> findByUserProjectId(long userId, long projectId) {
		String sql = "SELECT DISTINCT zone"
				+ " FROM Zone AS zone, UserZone AS userZone"
				+ " WHERE userZone.user.id = :userId"
				+ " AND userZone.zone.id = zone.id"
				+ " AND userZone.zone.project.id = :projectId"
				+ " AND userZone.role = true"
				+ " ORDER BY zone.area.order, zone.area.name, zone.order, zone.name";
	
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("userId", userId)
					.setParameter("projectId", projectId);
		
		return findMany(query);
	}

}
