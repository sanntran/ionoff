package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.DashboardDevice;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.IDashboardDeviceDao;

@Repository
@Transactional
public class DashboardDeviceDaoImpl extends AbstractGenericDao<DashboardDevice> implements IDashboardDeviceDao {

	@Autowired
	public DashboardDeviceDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(DashboardDevice.class);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<DashboardDevice> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public DashboardDevice findByDashboardDeviceId(long dashboardId, long deviceId) {
		String sql = "select distinct dashboardDevice"
				+ " from DashboardDevice as dashboardDevice"
				+ " where dashboardDevice.dashboard.id = :dashboardId"
				+ " and dashboardDevice.device.id = :deviceId";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("dashboardId", dashboardId)
				.setParameter("deviceId", deviceId)
				.setCacheable(true);
		return getFirst(findMany(query));
	}
}
