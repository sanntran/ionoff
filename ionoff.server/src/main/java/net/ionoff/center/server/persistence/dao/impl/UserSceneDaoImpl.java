package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.UserScene;
import net.ionoff.center.server.entity.UserZone;
import net.ionoff.center.server.persistence.dao.IUserSceneDao;

@Repository
@Transactional
public class UserSceneDaoImpl extends AbstractGenericDao<UserScene> implements IUserSceneDao {

	@Autowired
	public UserSceneDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(UserScene.class);
	}

	@Override
	public List<UserScene> findByProjectId(Long userId, Long projectId) {
		String sql = "SELECT DISTINCT userScene"
				+ " FROM UserScene AS userScene"
				+ " WHERE userScene.user.id = :userId"
				+ " AND userScene.project.id = :projectId"
				+ " ORDER BY userScene.scene.zone.order";
	
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("userId", userId)
					.setParameter("projectId", projectId);
		
		return findMany(query);
	}

	@Override
	public void removeByUserProjectId(long userId, long projectId) {
		String sql = "DELETE FROM UserScene AS userScene"
				+ " WHERE userScene.user.id= :userId"
				+ " AND userScene.project.id= :projectId";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("userId", userId)
				.setParameter("projectId", projectId);
	
		query.executeUpdate();
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<UserScene> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<UserScene> findByZoneId(Long userId, Long zoneId) {
		String sql = "SELECT DISTINCT userScene"
				+ " FROM UserScene AS userScene"
				+ " WHERE userScene.user.id = :userId"
				+ " AND userScene.scene.zone.id = :zoneId"
				+ " ORDER BY userScene.scene.name";
	
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("userId", userId)
					.setParameter("zoneId", zoneId);
		
		return findMany(query);
	}

	@Override
	public void removeByUserZone(long userId, long zoneId) {
		String sql = "DELETE FROM UserScene AS userScene"
				+ " WHERE userScene.user.id= :userId"
				+ " AND userScene.scene.zone.id= :zoneId";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("userId", userId)
				.setParameter("zoneId", zoneId);
	
		query.executeUpdate();
	}

	@Override
	public void updateRoleByUserZone(UserZone userZone) {
		String sql = "UPDATE UserScene as userScene SET userScene.role = :role" 
				+ " WHERE userScene.scene.id IN"
				+ " (SELECT scene.id FROM Scene AS scene WHERE scene.zone.id = :zoneId)"
				+ " AND userScene.user.id = :userId";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("role", userZone.getRole())
				.setParameter("zoneId", userZone.getZone().getId())
				.setParameter("userId", userZone.getUser().getId());
	
		query.executeUpdate();
		
	}
}
