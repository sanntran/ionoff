package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Dashboard;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.IDashboardDao;

@Repository
@Transactional
public class DashboardDaoImpl extends AbstractGenericDao<Dashboard> implements IDashboardDao {

	@Autowired
	public DashboardDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Dashboard.class);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<Dashboard> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Dashboard findByUserZoneId(long userId, long zoneId) {
		String sql = "SELECT DISTINCT dashboard"
				+ " FROM Dashboard AS dashboard"
				+ " WHERE dashboard.user.id = :userId"
				+ " AND dashboard.zone.id = :zoneId";
	
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("userId", userId)
					.setParameter("zoneId", zoneId);
		
		return getFirst(findMany(query));
	}

	@Override
	public Dashboard findByUserProjectId(long userId, long projectId) {
		String sql = "SELECT DISTINCT dashboard"
				+ " FROM Dashboard AS dashboard"
				+ " WHERE dashboard.user.id = :userId"
				+ " AND dashboard.project.id = :projectId"
				+ " AND dashboard.zone.id IS NULL";
	
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("userId", userId)
					.setParameter("projectId", projectId);
		
		return getFirst(findMany(query));
	}

	@Override
	public void removeByUserProjectId(long userId, long projectId) {
		String sql = "DELETE FROM Dashboard AS dashboard"
				+ " WHERE dashboard.user.id= :userId"
				+ " AND dashboard.project.id= :projectId";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("userId", userId)
				.setParameter("projectId", projectId);
	
		query.executeUpdate();
	}
}
