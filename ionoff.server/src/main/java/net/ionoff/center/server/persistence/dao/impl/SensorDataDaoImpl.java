package net.ionoff.center.server.persistence.dao.impl;

import java.text.ParseException;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import net.ionoff.center.server.entity.*;
import net.ionoff.center.server.persistence.dao.IDeviceDao;
import net.ionoff.center.server.util.DateTimeUtil;
import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.persistence.dao.ISensorDataDao;

@Transactional
public class SensorDataDaoImpl extends AbstractGenericDao<SensorData> implements ISensorDataDao {

	@Autowired
	private IDeviceDao deviceDao;

	public SensorDataDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(SensorData.class);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		Sensor sensor = getSensor(criteria);
		if (sensor == null) {
			return 0;
		}
		try {
			String dateTimes[] = criteria.getSearchKey().split("-");
			if (dateTimes.length != 2) {
				return 0;
			}
			Date fromDate = DateTimeUtil.yyyyMMddHHmmssFormatter.parse(dateTimes[0]);
			Date toDate = DateTimeUtil.yyyyMMddHHmmssFormatter.parse(dateTimes[1]);

			String sql = "SELECT COUNT(sensorData)"
					+ " FROM SensorData AS sensorData"
					+ " WHERE sensorData.time BETWEEN :fromDate AND :toDate"
					+ " AND sensorData.sensor.id = :sensorId";
			Query query = getCurrentSession().createQuery(sql)
					.setTimestamp("fromDate", fromDate)
					.setTimestamp("toDate", toDate)
					.setParameter("sensorId", sensor.getId());
			return countObjects(query);

		} catch (ParseException e) {
			throw new RuntimeException("Invalid date time");
		}
	}

	private Sensor getSensor(QueryCriteria criteria) {
		if (criteria.getDeviceId() == null) {
			return null;
		}
		Device device = deviceDao.findById(criteria.getDeviceId());
		if (device == null || !EntityUtil.isInstance(device, WeighScale.class)) {
			return null;
		}
		WeighScale scale = EntityUtil.castUnproxy(device, WeighScale.class);
		return scale.hasSensor() ? scale.getSensors().get(0) : null;
	}

	@Override
	public List<SensorData> findByCriteria(QueryCriteria criteria) {
		Sensor sensor = getSensor(criteria);
		if (sensor == null) {
			return Collections.emptyList();
		}
		try {
			String dateTimes[] = criteria.getSearchKey().split("-");
			if (dateTimes.length != 2) {
				Collections.emptyList();
			}
			Date fromDate = DateTimeUtil.yyyyMMddHHmmssFormatter.parse(dateTimes[0]);
			Date toDate = DateTimeUtil.yyyyMMddHHmmssFormatter.parse(dateTimes[1]);

			String sql = "SELECT DISTINCT sensorData"
					+ " FROM SensorData AS sensorData"
					+ " WHERE sensorData.time BETWEEN :fromDate AND :toDate"
					+ " AND sensorData.sensor.id = :sensorId";
			Query query = getCurrentSession().createQuery(sql)
					.setTimestamp("fromDate", fromDate)
					.setTimestamp("toDate", toDate)
					.setParameter("sensorId", sensor.getId());
			return findMany(query, criteria.getFromIndex(), criteria.getMaxResults());

		} catch (ParseException e) {
			throw new RuntimeException("Invalid date time");
		}
	}
}
