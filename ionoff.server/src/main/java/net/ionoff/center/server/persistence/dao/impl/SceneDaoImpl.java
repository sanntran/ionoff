package net.ionoff.center.server.persistence.dao.impl;

import java.util.Collections;
import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.Scene;
import net.ionoff.center.server.persistence.dao.ISceneDao;

@Transactional
public class SceneDaoImpl extends AbstractGenericDao<Scene> implements ISceneDao {
	
	public SceneDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Scene.class);
	}

	private long countByProjectId(long projectId) {
		String sql = "select count(scene)"
				+ " from Scene as scene"
				+ " where scene.zone.project.id = :projectId";
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("projectId", projectId);
		return countObjects(query);
	}
	
	private long countByProjectIdZoneName(long projectId, String keyWord) {
		String sql = "select count(scene)"
				+ " from Scene as scene"
				+ " where scene.zone.project.id = :projectId"
				+ " and lower(scene.zone.name) like :keyWord";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord.toLowerCase() + "%");
		return countObjects(query);
	}

	private long countByProjectIdName(long projectId, String keyWord) {
		String sql = "select count(scene)"
				+ " from Scene as scene"
				+ " where scene.zone.project.id = :projectId"
				+ " and lower(scene.name like) :keyWord";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord.toLowerCase() + "%");
		return countObjects(query);
	}

	private List<Scene> findByProjectIdZoneName(long projectId, String keyWord, int fromIndex, int maxResults, 
			String sortBy, boolean isAscending) {
		String sql = "select distinct scene" 
				+ " from Scene as scene" 
				+ " where scene.zone.project.id = :projectId"
				+ " and lower(scene.zone.name) like :keyWord" 
				+ " order by scene." + sortBy;
		if (!isAscending) {
			sql = sql + " desc";
		}
		
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord.toLowerCase() + "%");
		return findMany(query, fromIndex, maxResults);
	}

	private List<Scene> findByProjectIdName(long projectId, String keyWord, int fromIndex, int maxResults, 
			String sortBy, boolean isAscending) {
		String sql = "select distinct scene" 
					+ " from Scene as scene" 
					+ " where scene.zone.project.id = :projectId"
					+ " and lower(scene.name) like :keyWord" 
					+ " order by scene." + sortBy;
		if (!isAscending) {
			sql = sql + " desc";
		}
		
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord.toLowerCase() + "%");
		return findMany(query, fromIndex, maxResults);
	}
	
	private List<Scene> findByProjectId(long projectId, int fromIndex, int maxResults, 
			String sortBy, boolean isAscending) {
		String sql = "select distinct scene"
					+ " from Scene as scene"
					+ " where scene.zone.project.id = :projectId"
					+ " order by scene." + sortBy;
		
		if (!isAscending) {
			sql = sql + " desc";
		}
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("projectId", projectId);
		return findMany(query, fromIndex, maxResults);
	}

	@Override
	public List<Scene> findByProjectId(long projectId) {
		String sql = "select distinct scene"
				+ " from Scene as scene"
				+ " where scene.zone.project.id = :projectId"
				+ " order by scene.order, scene.name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId);
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
		if (ZONE_NAME.equals(criteria.getSearchField())) {
			return countByProjectIdZoneName(criteria.getProjectId(), criteria.getSearchKey());
		}
		return 0;
	}

	@Override
	public List<Scene> findByCriteria(QueryCriteria criteria) {
		if (criteria.isBlankKey()) {
			return findByProjectId(criteria.getProjectId(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
		}
		if (NAME.equals(criteria.getSearchField())) {
			return findByProjectIdName(criteria.getProjectId(), criteria.getSearchKey(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
		}
		if (ZONE_NAME.equals(criteria.getSearchField())) {
			return findByProjectIdZoneName(criteria.getProjectId(), criteria.getSearchKey(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
		}
		return Collections.emptyList();
	}

	@Override
	public List<Scene> findByUserZoneId(long userId, long zoneId) {
		
		String sql = "select distinct scene"
				+ " from Scene as scene, UserScene as userScene"
				+ " where scene.zone.id = :zoneId"
				+ " and userScene.scene.id = scene.id"
				+ " and userScene.user.id = :userId"
				+ " order by scene.order, scene.name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("zoneId", zoneId)
				.setParameter("userId", userId)
				.setCacheable(true);
		
		return findMany(query);
	}
	
	@Override
	public List<Scene> findByUserProjectId(long userId, Long projectId) {
		String sql = "select distinct scene"
				+ " from Scene as scene, UserScene as userScene"
				+ " where scene.id = userScene.scene.id"
				+ " and scene.zone.project.id = :projectId"
				+ " and userScene.user.id = :userId"
				+ " and userScene.role = true"
				+ " order by scene.order, scene.name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("userId", userId)
				.setParameter("projectId", projectId)
				.setCacheable(true);
		return findMany(query);
	}
}
