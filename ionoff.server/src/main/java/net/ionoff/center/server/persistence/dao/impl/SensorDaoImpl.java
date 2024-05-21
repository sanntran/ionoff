package net.ionoff.center.server.persistence.dao.impl;

import java.util.Collections;
import java.util.List;

import net.ionoff.center.server.entity.Controller;
import javax.persistence.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.persistence.dao.ISensorDao;

@Repository
@Transactional
public class SensorDaoImpl extends AbstractGenericDao<Sensor> implements ISensorDao {

	public SensorDaoImpl() {
		super();
		setClass(Sensor.class);
	}

	public long countByName(long projectId, String name) {
		if (name == null || name.isEmpty()) {
			String sql = "select count(sensor)"
						+ " from Sensor as sensor"
						+ " where sensor.project.id = :projectId";
			Query query = entityManager.createQuery(sql)
						.setParameter("projectId", projectId);;
			return countObjects(query);
			
		}
		String sql = "select count(sensor)"
					+ " from Sensor as sensor" 
					+ " where sensor.project.id = :projectId"
					+ " and lower(sensor.name) like :name";
		Query query = entityManager.createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("name", "%" + name.toLowerCase() + "%");
		return countObjects(query);
	}
	
	public List<Sensor> findByName(long projectId, String name, 
						int fromIndex, int maxResults, String sortBy, boolean isAscending) {
		if (name == null || name.isEmpty()) {
			return findAll(projectId, fromIndex, maxResults, isAscending);
		}
		String sql = "select distinct sensor" 
					+ " from Sensor as sensor" 
					+ " where sensor.project.id = :projectId"
					+ " and lower(sensor.name) like :name" 
					+ " order by sensor." + sortBy;
		if (!isAscending) {
			sql = sql + " desc";
		}
		
		Query query = entityManager.createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("name", "%" + name.toLowerCase() + "%");
		return findMany(query, fromIndex, maxResults);
	}

	private List<Sensor> findAll(long projectId, int fromIndex, int maxResults, boolean isAscending) {
		String sql = "select distinct sensor"
					+ " from Sensor as sensor"
					+ " where sensor.project.id = :projectId"
					+ " order by sensor.order, sensor.name";
		if (!isAscending) {
			sql = sql + " desc";
		}
		Query query = entityManager.createQuery(sql)
					.setParameter("projectId", projectId);
		return findMany(query, fromIndex, maxResults);
	}

	@Override
	public List<Sensor> findByProjectId(long projectId) {
		String sql = "select distinct sensor"
				+ " from Sensor as sensor"
				+ " where sensor.project.id = :projectId"
				+ " order by sensor.order, sensor.name";
		Query query = entityManager.createQuery(sql)
				.setParameter("projectId", projectId);
		return findMany(query);
	}

	@Override
	public List<Sensor> findOfflineInProject(long projectId) {
		String sql = "SELECT s.* FROM ionoff.sensors s " +
				" JOIN ionoff.controllers c ON c.id = s.controller_id " +
				" WHERE c.project_id = :projectId" +
				" AND (c.connection_expired IS NULL OR c.connection_expired <= UNIX_TIMESTAMP() * 1000)";
		Query query = entityManager.createNativeQuery(sql, Sensor.class)
				.setParameter("projectId", projectId);
		return findMany(query);
	}

	@Override
	public List<Sensor> findHavingAlertInProject(long projectId) {
		String sql = "SELECT s.* FROM ionoff.sensors s " +
				" JOIN ionoff.sensors_status ss ON ss.id = s.id " +
				" WHERE s.project_id = :projectId AND ss.alert = 1";
		Query query = entityManager.createNativeQuery(sql, Sensor.class)
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
	public List<Sensor> findByCriteria(QueryCriteria criteria) {
		if (NAME.equals(criteria.getSearchField())) {
			return findByName(criteria.getProjectId(), criteria.getSearchKey(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
		}
		return Collections.emptyList();
	}

	@Override
	public List<Sensor> findByDeviceId(long deviceId) {
		String sql = "select distinct sensor"
				+ " from Sensor as sensor"
				+ " where sensor.device.id = :deviceId"
				+ " order by sensor.order, sensor.name";
		Query query = entityManager.createQuery(sql)
				.setParameter("deviceId", deviceId);
		return findMany(query);
	}
}
