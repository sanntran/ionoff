package net.ionoff.center.server.persistence.dao.impl;

import java.util.Collections;
import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.Player;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.WeighScale;
import net.ionoff.center.server.persistence.dao.IDeviceDao;

@Transactional
public class DeviceDaoImpl extends AbstractGenericDao<Device> implements IDeviceDao {

	public DeviceDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(Device.class);
	}

	@Override
	public Player findPlayerByMac(String mac) {
		
		String sql = "select distinct player"
					+ " from net.ionoff.center.server.entity.Player as player"
					+ " where player.mac = :mac";
		
		Query query = getCurrentSession().createQuery(sql)						
					.setString("mac", mac);
		
		List<Device> devices = findMany(query);	
		
		Device device = getFirst(devices);
		
		if (device != null) {
			return (Player) device;
		}
		
		return null;
	}
	
	@Override
	public WeighScale findWeighScaleByMac(String mac) {
		
		String sql = "select distinct scale"
					+ " from net.ionoff.center.server.entity.WeighScale as scale"
					+ " where scale.mac = :mac";
		
		Query query = getCurrentSession().createQuery(sql)						
					.setString("mac", mac);
		
		List<Device> devices = findMany(query);	
		
		Device device = getFirst(devices);
		
		if (device != null) {
			return (WeighScale) device;
		}
		
		return null;
	}
	
	@Override
	public long countByCriteria(QueryCriteria criteria) {
		if (criteria.isBlankKey()) {
			return countByProjectId(criteria.getProjectId());
		}
		if (NAME.equals(criteria.getSearchField())) {
			return countByProjectIdName(criteria.getProjectId(), criteria.getSearchKey());
		}
		if (ZONE_NAME.equals(criteria.getSearchField())) { 
			return countByProjectIdZoneName(criteria.getProjectId(), criteria.getSearchKey());
		}
		return 0;
	}
	
	@Override
	public long countByProjectId(long projectId) {
		String sql = "select count(device)"
				+ " from Device as device"
				+ " where device.project.id = :projectId";
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("projectId", projectId);
		return countObjects(query);
	}

	private long countByProjectIdName(long projectId, String name) {
		String sql = "select count(device)"
				+ " from Device as device"
				+ " where device.project.id = :projectId"
				+ " and lower(device.name) like :name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("name", "%" + name.toLowerCase() + "%");
		return countObjects(query);
	}

	private long countByProjectIdZoneName(long projectId, String zoneName) {
		String sql = "select count(device)"
				+ " from Device as device"
				+ " where device.project.id = :projectId"
				+ " and lower(device.zone.name) like :zoneName";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("zoneName", "%" + zoneName.toLowerCase() + "%");
		return countObjects(query);
	}

	@Override
	public List<Device> findByCriteria(QueryCriteria criteria) {
		if (criteria.isBlankKey()) {
			return findByProjectId(criteria.getProjectId(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
		}
		if (NAME.equals(criteria.getSearchField())) {
			return findByProjectIdName(criteria.getProjectId(), criteria.getSearchKey(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
		}
		if (ZONE_NAME.equals(criteria.getSearchField())) { 
			return findByProjectIdZoneName(criteria.getProjectId(), criteria.getSearchKey(), 
					criteria.getFromIndex(), criteria.getMaxResults(), criteria.getSortBy(), criteria.getIsAscending());
		}
		return Collections.emptyList();
	}

	private List<Device> findByProjectIdZoneName(long projectId, String keyWord, int fromIndex, int maxResults, 
			String sortBy, boolean isAscending) {
		String sql = "select distinct device" 
				+ " from Device as device" 
				+ " where device.project.id = :projectId"
				+ " and device.zone.name like :keyWord" 
				+ " order by device." + sortBy;
		if (!isAscending) {
			sql = sql + " desc" + ", device.order";
		}
		else {
			sql = sql + ", device.order";
		}
		
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord + "%");
		return findMany(query, fromIndex, maxResults);
	}

	private List<Device> findByProjectIdName(long projectId, String keyWord, int fromIndex, int maxResults, 
			String sortBy, boolean isAscending) {
		String sql = "select distinct device" 
				+ " from Device as device" 
				+ " where device.project.id = :projectId"
				+ " and device.name like :keyWord" 
				+ " order by device." + sortBy;
		if (!isAscending) {
			sql = sql + " desc" + ", device.order";
		}
		else {
			sql = sql + ", device.order";
		}
		
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("projectId", projectId)
				.setParameter("keyWord", "%" + keyWord + "%");
		return findMany(query, fromIndex, maxResults);
	}

	private List<Device> findByProjectId(long projectId, int fromIndex, int maxResults, 
			String sortBy, boolean isAscending) {
		String sql = "select distinct device"
					+ " from Device as device"
					+ " where device.project.id = :projectId"
					+ " order by device." + sortBy;
		
		if (!isAscending) {
			sql = sql + " desc" + ", device.order";
		}
		else {
			sql = sql + ", device.order";
		}
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("projectId", projectId);
		
		return findMany(query, fromIndex, maxResults);
	}

	@Override
	public List<Device> findByZoneId(long userId, long zoneId) {
		String sql = "select distinct device"
				+ " from Device as device, UserDevice as userDevice"
				+ " where userDevice.device.id = device.id"
				+ " and device.zone.id = :zoneId"
				+ " and userDevice.role = true"
				+ " order by device.order, device.name";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("zoneId", zoneId)
				.setCacheable(true);
		
		return findMany(query);
	}

	@Override
	public List<Device> findByUserProjectId(long userId, long projectId) {
		
		String sql = "select distinct device"
				+ " from Device as device, UserDevice as userDevice"
				+ " where userDevice.device.id = device.id"
				+ " and userDevice.user.id = :userId"
				+ " and userDevice.project.id = :projectId"
				+ " and userDevice.role = true"
				+ " order by device.zone.area.order, device.zone.area.name,"
				+ " device.zone.order, device.zone.name, device.order, device.name";
	
		Query query = getCurrentSession().createQuery(sql)
					.setParameter("userId", userId)
					.setParameter("projectId", projectId)
					.setCacheable(true);
		
		 List<Device> devices = findMany(query);
		 return devices;
	}

}
