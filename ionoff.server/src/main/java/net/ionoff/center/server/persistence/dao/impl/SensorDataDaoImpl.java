package net.ionoff.center.server.persistence.dao.impl;

import java.text.ParseException;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.EntityUtil;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.SensorData;
import net.ionoff.center.server.entity.SensorDriver;
import net.ionoff.center.server.persistence.dao.IDeviceDao;
import net.ionoff.center.server.persistence.dao.ISensorDataDao;
import net.ionoff.center.server.util.DateTimeUtil;

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

	@Override
	public long countByDay(QueryCriteria criteria) {

		try {
			String dateTimes[] = criteria.getSearchKey().split("-");
			if (dateTimes.length != 2) {
				return 0;
			}
			Date fromDate = DateTimeUtil.yyyyMMddHHmmssFormatter.parse(dateTimes[0]);
			Date toDate = DateTimeUtil.yyyyMMddHHmmssFormatter.parse(dateTimes[1]);

			long period = toDate.getTime() - fromDate.getTime();
			long days = TimeUnit.DAYS.convert(period, TimeUnit.MILLISECONDS) + 1;

			return days;

		} catch (ParseException e) {
			throw new RuntimeException("Invalid date time");
		}
	}

	private Sensor getSensor(QueryCriteria criteria) {
		if (criteria.getDeviceId() == null) {
			return null;
		}
		Device device = deviceDao.findById(criteria.getDeviceId());
		if (device == null || !EntityUtil.isInstance(device, SensorDriver.class)) {
			return null;
		}
		SensorDriver sensorDriver = EntityUtil.castUnproxy(device, SensorDriver.class);
		return sensorDriver.hasSensor() ? sensorDriver.getSensors().get(0) : null;
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

	@Override
	public List<SensorData> findByDay(QueryCriteria criteria) {
		try {
			Sensor sensor = getSensor(criteria);
			if (sensor == null) {
				return Collections.emptyList();
			}
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
			return findMany(query);

		} catch (ParseException e) {
			throw new RuntimeException("Invalid date time");
		}
	}
}
