package net.ionoff.center.server.persistence.dao.impl;

import net.ionoff.center.server.entity.Area;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.IAreaDao;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import javax.persistence.Query;
import java.util.Collections;
import java.util.List;

@Repository
@Transactional
public class AreaDaoImpl extends AbstractGenericDao<Area> implements IAreaDao {

	public AreaDaoImpl() {
		super();
		setClass(Area.class);
	}

	private long countByName(long projectId, String name) {
		if (name == null || name.isEmpty()) {
			return countByProjectId(projectId);
		}
		String sql = "select count(area)"
					+ " from Area as area" 
					+ " where area.project.id = :projectId"
					+ " and lower(area.name) like :name";
		Query query = entityManager.createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("name", "%" + name.toLowerCase() + "%");
		return countObjects(query);
	}
	
	private long countByProjectId(long projectId) {
		String sql = "select count(area)"
				+ " from Area as area"
				+ " where area.project.id = :projectId";
		Query query = entityManager.createQuery(sql)
					.setParameter("projectId", projectId);;
		return countObjects(query);
	}

	private List<Area> findByName(long projectId, String name, 
						int fromIndex, int maxResults, String sortBy, boolean isAscending) {
		if (name == null || name.isEmpty()) {
			return findByProjectId(projectId, fromIndex, maxResults, isAscending);
		}
		String sql = "select distinct area" 
					+ " from Area as area" 
					+ " where area.project.id = :projectId"
					+ " and lower(area.name) like :name" 
					+ " order by area." + sortBy;
		if (!isAscending) {
			sql = sql + " desc";
		}
		
		Query query = entityManager.createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("name", "%" + name.toLowerCase() + "%");
		return findMany(query, fromIndex, maxResults);
	}

	private List<Area> findByProjectId(long projectId, int fromIndex, int maxResults, boolean isAscending) {
		String sql = "select distinct area"
					+ " from Area as area"
					+ " where area.project.id = :projectId"
					+ " order by area.order, area.name";
		if (!isAscending) {
			sql = sql + " desc";
		}
		Query query = entityManager.createQuery(sql)
					.setParameter("projectId", projectId);
		return findMany(query, fromIndex, maxResults);
	}

	@Override
	public List<Area> findByProjectId(long projectId) {
		String sql = "select distinct area"
				+ " from Area as area"
				+ " where area.project.id = :projectId"
				+ " order by area.order, area.name";
		Query query = entityManager.createQuery(sql)
				.setParameter("projectId", projectId);
		return findMany(query);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		if (NAME.equals(criteria.getSearchField())) {
			return countByName(criteria.getProjectId(), criteria.getSearchKey());
		}
		return 0;
	}

	@Override
	public List<Area> findByCriteria(QueryCriteria criteria) {
		if (NAME.equals(criteria.getSearchField())) {
			return findByName(criteria.getProjectId(), criteria.getSearchKey(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
		}
		return Collections.emptyList();
	}
}
