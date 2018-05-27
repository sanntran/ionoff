package net.ionoff.center.server.persistence.dao.impl;

import java.text.DecimalFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.apache.log4j.Logger;
import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.EntityUtil;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.Sensor;
import net.ionoff.center.server.entity.SensorData;
import net.ionoff.center.server.entity.WeighScale;
import net.ionoff.center.server.persistence.dao.IDeviceDao;
import net.ionoff.center.server.persistence.dao.ISensorDataDao;
import net.ionoff.center.server.util.DateTimeUtil;

@Transactional
public class SensorDataDaoImpl extends AbstractGenericDao<SensorData> implements ISensorDataDao {

	private static final Logger logger = Logger.getLogger(SensorDataDaoImpl.class.getName());
	
	private static final DecimalFormat TWO_DFORM = new DecimalFormat("#.##");

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


			long period = toDate.getTime() - fromDate.getTime();
			long days = TimeUnit.DAYS.convert(period, TimeUnit.MILLISECONDS) + 1;

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

	@Override
	public List<SensorData> getSumByDay(QueryCriteria criteria) {
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

			long period = toDate.getTime() - fromDate.getTime();
			long days = TimeUnit.DAYS.convert(period, TimeUnit.MILLISECONDS) + 1;

			List<SensorData> results = new ArrayList<>();

			if (criteria.getFromIndex() >= days) {
				return results;
			}

			final String sql = "SELECT YEAR(sensorData.time), MONTH(sensorData.time), DAY(sensorData.time)," +
					" SUM(sensorData.value), MAX(sensorData.index)" +
					" FROM SensorData as sensorData" +
					" WHERE sensorData.sensor.id = :sensorId" +
					" AND sensorData.time BETWEEN :fromDate AND :toDate" +
					" GROUP BY DAY(sensorData.time)";

			Query query = getCurrentSession().createQuery(sql)
					.setTimestamp("fromDate", fromDate)
					.setTimestamp("toDate", toDate)
					.setParameter("sensorId", sensor.getId());

			List<SensorData> records = findMany(query);

			for (int i = 0; i < days && i < Integer.MAX_VALUE; i++) {
				SensorData data = new SensorData();
				Calendar cal = Calendar.getInstance();
				cal.setTime(toDate);
				cal.add(Calendar.DATE, -i);
				cal.set(Calendar.HOUR_OF_DAY, 23);
				cal.set(Calendar.MINUTE, 59);
				cal.set(Calendar.SECOND, 59);
				data.setId(i + 1);
				data.setTime(cal.getTime());
				data.setValue(0D);
				data.setIndex(0L);
				data.setSensor(sensor);
				results.add(data);
			}

			Calendar cal = Calendar.getInstance();

			for (Object object : records) {
				Object[] objArr = (Object[]) object;
				int year = (Integer) objArr[0];
				int month = (Integer) objArr[1];
				int day = (Integer) objArr[2];

				for (SensorData data : results) {
					cal.setTime(data.getTime());
					int y = cal.get(Calendar.YEAR);
					int m = cal.get(Calendar.MONTH) + 1;
					int d = cal.get(Calendar.DAY_OF_MONTH);
					if (y == year && m == month && d == day) {
						double val = Double.valueOf(TWO_DFORM.format(objArr[3]));
						data.setValue(val);
						data.setIndex((Long) objArr[4]);
						break;
					}
				}
			}

			if (criteria.getFromIndex() + criteria.getMaxResults() > results.size()) {
				return results.subList(criteria.getFromIndex(), results.size());
			}
			return results.subList(criteria.getFromIndex(), criteria.getFromIndex() + criteria.getMaxResults());

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
