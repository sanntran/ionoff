package net.ionoff.center.server.persistence.dao.impl;

import java.text.ParseException;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.SensorData;
import net.ionoff.center.server.persistence.dao.IDeviceDao;
import net.ionoff.center.server.persistence.dao.ISensorDataDao;
import net.ionoff.center.server.util.DateTimeUtil;

@Repository
@Transactional
public class SensorDataDaoImpl extends AbstractGenericDao<SensorData> implements ISensorDataDao {

	@Autowired
	private IDeviceDao deviceDao;

	@Autowired
	public SensorDataDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(SensorData.class);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		return 0;
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

	@Override
	public List<SensorData> findByCriteria(QueryCriteria criteria) {
		return Collections.emptyList();
	}

	@Override
	public List<SensorData> findByDay(QueryCriteria criteria) {
		return Collections.emptyList();
	}
}
