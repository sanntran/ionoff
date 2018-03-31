package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.UserZone;
import net.ionoff.center.server.persistence.dao.IUserZoneDao;

@Transactional
public class UserZoneDaoImpl extends AbstractGenericDao<UserZone> implements IUserZoneDao {

	public UserZoneDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(UserZone.class);
	}

	@Override
	public List<UserZone> findByProjectId(Long userId, Long projectId) {
		String sql = "SELECT DISTINCT userZone"
				+ " FROM UserZone AS userZone"
				+ " WHERE userZone.user.id = :userId"
				+ " AND userZone.zone.project.id = :projectId"
				+ " ORDER BY userZone.zone.area.order, userZone.zone.area.name,"
				+ " userZone.zone.order, userZone.zone.name";
	
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("userId", userId)
					.setParameter("projectId", projectId);
		
		return findMany(query);
	}

	@Override
	public void removeByUserProjectId(long userId, long projectId) {
		String sql = "DELETE FROM UserZone AS userZone"
				+ " WHERE userZone.user.id= :userId"
				+ " AND userZone.project.id= :projectId";
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
	public List<UserZone> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@SuppressWarnings("unchecked")
	@Override
	public UserZone findByUserZone(long userId, long zoneId) {
		String sql = "SELECT userZone"
				+ " FROM UserZone AS userZone"
				+ " WHERE userZone.user.id = :userId"
				+ " AND userZone.zone.id = :zoneId";
	
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("userId", userId)
					.setParameter("zoneId", zoneId);
		
		return getFirst(query.list());
	}
}
