package net.ionoff.center.server.persistence.dao.impl;

import java.util.Collections;
import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.persistence.dao.IRelayDao;

@Transactional
public class RelayDaoImpl extends AbstractGenericDao<Relay> implements IRelayDao {

	public static final String DRIVER_NAME = "driverName";

	public RelayDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Relay.class);
	}

	@Override
	public synchronized Relay update(Relay relay) {
		return super.update(relay);
		
	}
	
	private long countByProjectIdRelayDriverName(long projectId, String driverName) {
		String sql = "select count(relay)"
				+ " from Relay as relay" 
				+ " where relay.driver.project.id = :projectId"
				+ " and lower(relay.driver.name) like :driverName";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("driverName", "%" + driverName.toLowerCase() + "%");
		return countObjects(query);
	}
	
	public long countByName(long projectId, String name) {
		if (name == null || name.isEmpty()) {
			return countByProjectId(projectId);
		}
		String sql = "select count(relay)"
					+ " from Relay as relay" 
					+ " where relay.driver.project.id = :projectId"
					+ " and lower(relay.name) like :name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("name", "%" + name.toLowerCase() + "%");
		return countObjects(query);
	}
	
	private long countByProjectId(long projectId) {
		String sql = "select count(relay)"
				+ " from Relay as relay"
				+ " where relay.driver.project.id = :projectId";
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("projectId", projectId);;
		return countObjects(query);
	}

	private long countByProjectIdDeviceName(long projectId, String deviceName) {
		String sql = "select count(relay)"
					+ " from Relay as relay" 
					+ " where relay.driver.project.id = :projectId"
					+ " and lower(relay.device.name) like :deviceName";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("deviceName", "%" + deviceName.toLowerCase() + "%");
		return countObjects(query);
	}
	
	
	private List<Relay> findByProjectIdRelayDriverName(long projectId, String driverName, int fromIndex, int maxResults,
			String sortBy, boolean isAscending) {
		
		String sql = "select distinct relay" 
				+ " from Relay as relay" 
				+ " where relay.driver.project.id = :projectId"
				+ " and lower(relay.driver.name) like :driverName" 
				+ " order by relay." + sortBy;
		if (!isAscending) {
			sql = sql + " desc"  + ", relay.index";
		}
		else {
			sql = sql + ", relay.index";
		}
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("driverName", "%" + driverName.toLowerCase() + "%");
		return findMany(query, fromIndex, maxResults);
	}

	private List<Relay> findByProjectIdDeviceName(long projectId, String deviceName, int fromIndex, int maxResults,
			String sortBy, boolean isAscending) {
		String sql = "select distinct relay" 
				+ " from Relay as relay" 
				+ " where relay.driver.project.id = :projectId"
				+ " and lower(relay.device.name) like :deviceName" 
				+ " order by relay." + sortBy;
		if (!isAscending) {
			sql = sql + " desc"  + ", relay.index";
		}
		else {
			sql = sql + ", relay.index";
		}
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("deviceName", "%" + deviceName.toLowerCase() + "%");
		return findMany(query, fromIndex, maxResults);
	}

	public List<Relay> findByProjectIdName(long projectId, String name, 
						int fromIndex, int maxResults, String sortBy, boolean isAscending) {
		if (name == null || name.isEmpty()) {
			return findByProjectId(projectId, fromIndex, maxResults, sortBy, isAscending);
		}
		String sql = "select distinct relay" 
					+ " from Relay as relay" 
					+ " where relay.driver.project.id = :projectId"
					+ " and lower(relay.name) like :name" 
					+ " order by relay." + sortBy;
		if (!isAscending) {
			sql = sql + " desc"  + ", relay.index";
		}
		else {
			sql = sql + ", relay.index";
		}
		
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("name", "%" + name.toLowerCase() + "%");
		return findMany(query, fromIndex, maxResults);
	}

	private List<Relay> findByProjectId(long projectId, int fromIndex, int maxResults, String sortBy, boolean isAscending) {
		String sql = "select distinct relay"
					+ " from Relay as relay"
					+ " where relay.driver.project.id = :projectId"
					+ " order by relay." + sortBy;
		if (!isAscending) {
			sql = sql + " desc"  + ", relay.index";
		}
		else {
			sql = sql + ", relay.index";
		}
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("projectId", projectId);
		return findMany(query, fromIndex, maxResults);
	}

	@Override
	public List<Relay> findByProjectId(long projectId) {
		String sql = "select distinct relay"
				+ " from Relay as relay"
				+ " where relay.driver.project.id = :projectId"
				+ " order by relay.driver.id, relay.index";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId);
		return findMany(query);
	}

	@Override
	public List<Relay> findByRelayDriverId(long relayDriverId) {
		String sql = "select distinct relay"
				+ " from Relay as relay"
				+ " where relay.driver.id = :relayDriverId"
				+ " order by relay.index";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("relayDriverId", relayDriverId);
		return findMany(query);
	}

	@Override
	public List<Relay> findByDeviceId(long deviceId) {
		String sql = "select distinct relay"
				+ " from Relay as relay"
				+ " where relay.device.id = :deviceId"
				+ " order by relay.driver.id, relay.index";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("deviceId", deviceId);
		return findMany(query);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		if (criteria.isBlankKey()) {
			return countByProjectId(criteria.getProjectId());
		}
		if (NAME.equals(criteria.getSearchField())) {
			return countByName(criteria.getProjectId(), criteria.getSearchKey());
		}
		if (DEVICE_NAME.equals(criteria.getSearchField())) {
			return countByProjectIdDeviceName(criteria.getProjectId(), criteria.getSearchKey());
		}	
		if (DRIVER_NAME.equals(criteria.getSearchField())) {
			return countByProjectIdRelayDriverName(criteria.getProjectId(), criteria.getSearchKey());
		}
		return 0;
	}

	@Override
	public List<Relay> findByCriteria(QueryCriteria criteria) {
		if (criteria.isBlankKey()) {
			return findByProjectId(criteria.getProjectId(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
		}
		if (NAME.equals(criteria.getSearchField())) {
			return findByProjectIdName(criteria.getProjectId(), criteria.getSearchKey(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
		}
		if (DEVICE_NAME.equals(criteria.getSearchField())) {
			return findByProjectIdDeviceName(criteria.getProjectId(), criteria.getSearchKey(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
		}	
		if (DRIVER_NAME.equals(criteria.getSearchField())) {
			return findByProjectIdRelayDriverName(criteria.getProjectId(), criteria.getSearchKey(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
		}
		return Collections.emptyList();
	}
}
