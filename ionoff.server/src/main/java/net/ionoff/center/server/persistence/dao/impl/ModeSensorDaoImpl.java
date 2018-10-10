package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.persistence.dao.IModeSensorDao;

@Repository
@Transactional
public class ModeSensorDaoImpl extends AbstractGenericDao<ModeSensor> implements IModeSensorDao {

	@Autowired
	public ModeSensorDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(ModeSensor.class);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<ModeSensor> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<ModeSensor> findBySensorId(Long sensorId) {
		String sql = "select distinct modeSensor" 
				+ " from ModeSensor as modeSensor" 
				+ " where modeSensor.sensor.id = :sensorId";
		
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("sensorId", sensorId);
		return findMany(query);
	}

	@Override
	public List<ModeSensor> findOnSensorStatusChanged(Sensor sensor) {
		String sql = "SELECT DISTINCT modeSensor" 
				+ " FROM ModeSensor AS modeSensor, Mode as mode" 
				+ " WHERE modeSensor.sensor.id = :sensorId"
				+ " AND ((modeSensor.mode.id = mode.id AND mode.isActivated = true)"
					   + " OR (modeSensor.mode IS NULL))"
				+ " AND modeSensor.enabled = true";
		
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("sensorId", sensor.getId());
		
		return findMany(query);
	}
}
