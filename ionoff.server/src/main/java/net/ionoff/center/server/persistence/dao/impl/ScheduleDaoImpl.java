package net.ionoff.center.server.persistence.dao.impl;

import java.util.Collections;
import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.Schedule;
import net.ionoff.center.server.persistence.dao.IScheduleDao;

@Repository
@Transactional
public class ScheduleDaoImpl extends AbstractGenericDao<Schedule> implements IScheduleDao {

	@Autowired
	public ScheduleDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Schedule.class);
	}
	
	private long countByProjectId(long projectId) {
		String sql = "select count(scheduler)"
				+ " from Schedule as scheduler"
				+ " where scheduler.project.id = :projectId";
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("projectId", projectId);
		return countObjects(query);
	}

	private long countByProjectIdName(long projectId, String name) {
		String sql = "select count(scheduler)"
				+ " from Schedule as scheduler"
				+ " where scheduler.project.id = :projectId"
				+ " and lower(scheduler.name) like :name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("name", "%" + name + "%");
		return countObjects(query);
	}
	
	private long countByProjectIdDeviceName(long projectId, String keyWord) {
		String sql = "select count(scheduler)"
				+ " from Schedule as scheduler"
				+ " where scheduler.project.id = :projectId"
				+ " and lower(scheduler.device.name) like :keyWord";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord.toLowerCase() + "%");
		return countObjects(query);
	}

	private List<Schedule> findByProjectIdDeviceName(long projectId, String keyWord,
			int fromIndex, int maxResults, String sortBy, boolean isAscending) {
		String sql = "select distinct scheduler"
				+ " from Schedule as scheduler"
				+ " where scheduler.project.id = :projectId"
				+ " and lower(scheduler.device.name) like :keyWord"
				+ " order by scheduler." + sortBy;
		if (!isAscending) {
			sql = sql + " desc";
		}
		
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord.toLowerCase() + "%");
		return findMany(query, fromIndex, maxResults);
	}

	private List<Schedule> findByProjectIdName(long projectId, String keyWord, int fromIndex, int maxResults, 
			String sortBy, boolean isAscending) {
		String sql = "select distinct scheduler"
				+ " from Schedule as scheduler"
				+ " where scheduler.project.id = :projectId"
				+ " and lower(scheduler.name like) :name"
				+ " order by scheduler." + sortBy;
		if (!isAscending) {
			sql = sql + " desc";
		}
		
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("name", "%" + keyWord.toLowerCase() + "%");
		return findMany(query, fromIndex, maxResults);
	}

	@Override
	public List<Schedule> findByProjectId(long projectId) {
		
		String sql = "select distinct scheduler"
				+ " from Schedule as scheduler"
				+ " where scheduler.project.id = :projectId"
				+ " order by scheduler.name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId);
		
		return findMany(query);
	}
	
	private List<Schedule> findByProjectId(long projectId, int fromIndex, int maxResults, String sortBy, boolean isAscending) {
		String sql = "select distinct scheduler"
					+ " from Schedule as scheduler"
					+ " where scheduler.project.id = :projectId"
					+ " order by scheduler." + sortBy;
		
		if (!isAscending) {
			sql = sql + " desc";
		}
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("projectId", projectId);
		return findMany(query, fromIndex, maxResults);
	}

	@Override
	public List<Schedule> findByDeviceId(long deviceId) {
		String sql = "select distinct scheduler"
				+ " from Schedule as scheduler"
				+ " where scheduler.device.id = :deviceId"
				+ " order by scheduler.name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("deviceId", deviceId);
		
		return findMany(query);
	}

	@Override
	public List<Schedule> findFailedSchedules() {
		String sql = "select distinct scheduler"
				+ " from Schedule as scheduler"
				+ " where scheduler.enabled = :enabled"
				+ " and scheduler.status = :status"
				+ " order by scheduler.project.id";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("enabled", true)
				.setParameter("status", false);
		
		return findMany(query);
	}

	@Override
	public List<Schedule> findEnabledSchedules(String scheduleTime) {
		String sql = "select distinct scheduler"
				+ " from Schedule as scheduler"
				+ " where scheduler.enabled = :enabled"
				+ " and scheduler.time = :scheduleTime"
				+ " order by scheduler.project.id";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("enabled", true)
				.setParameter("scheduleTime", scheduleTime);
		
		return findMany(query);
	}

	@Override
	public List<Schedule> findByZoneId(long zoneId) {
		String sql = "select distinct scheduler"
				+ " from Schedule as scheduler"
				+ " where scheduler.device.zone.id = :zoneId";
	
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("zoneId", zoneId);
		
		 List<Schedule> schedules = findMany(query);
		 return schedules;
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		if (criteria.isBlankKey()) {
			return countByProjectId(criteria.getProjectId());
		}
		if (NAME.equals(criteria.getSearchField())) {
			return countByProjectIdName(criteria.getProjectId(), criteria.getSearchKey());
		}
		if (DEVICE_NAME.equals(criteria.getSearchField())) {
			return countByProjectIdDeviceName(criteria.getProjectId(), criteria.getSearchKey());
		}
		return 0;
	}

	@Override
	public List<Schedule> findByCriteria(QueryCriteria criteria) {

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
		return Collections.emptyList();
	}

	@Override
	public List<Schedule> findByUserZoneId(long userId, Long zoneId) {
		String sql = "select distinct scheduler"
				+ " from Schedule as scheduler, UserDevice as userDevice"
				+ " where userDevice.user.id = :userId"
				+ " and userDevice.device.id = scheduler.device.id"
				+ " and userDevice.device.zone.id = :zoneId"
				+ " and userDevice.role = true"
				+ " order by scheduler.name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("userId", userId)
				.setParameter("zoneId", zoneId)
				.setCacheable(true);
		return findMany(query);
	}


	@Override
	public List<Schedule> findByUserProjectId(long userId, long projectId) {
		String sql = "select distinct scheduler"
				+ " from Schedule as scheduler, UserDevice as userDevice"
				+ " where userDevice.user.id = :userId"
				+ " and userDevice.device.id = scheduler.device.id"
				+ " and userDevice.role = true"
				+ " and userDevice.project.id = :projectId"
				+ " order by scheduler.name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("userId", userId)
				.setParameter("projectId", projectId)
				.setCacheable(true);
		return findMany(query);
	}
	
}
