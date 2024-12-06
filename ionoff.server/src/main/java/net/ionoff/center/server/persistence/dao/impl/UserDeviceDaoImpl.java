package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import javax.persistence.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.UserDevice;
import net.ionoff.center.server.entity.UserZone;
import net.ionoff.center.server.persistence.dao.IUserDeviceDao;

@Repository
@Transactional
public class UserDeviceDaoImpl extends AbstractGenericDao<UserDevice> implements IUserDeviceDao {

	public UserDeviceDaoImpl() {
		super();
		setClass(UserDevice.class);
	}

	@Override
	public List<UserDevice> findByUserProjectId(Long userId, Long projectId) {
		String sql = "SELECT DISTINCT userDevice"
				+ " FROM UserDevice AS userDevice"
				+ " WHERE userDevice.user.id = :userId"
				+ " AND userDevice.project.id = :projectId"
				+ " ORDER BY userDevice.device.zone.order";
	
		Query query = entityManager.createQuery(sql)
					.setParameter("userId", userId)
					.setParameter("projectId", projectId);
		
		return findMany(query);
	}

	@Override
	public void removeByUserProjectId(long userId, long projectId) {
		String sql = "DELETE FROM UserDevice AS userDevice"
				+ " WHERE userDevice.user.id= :userId"
				+ " AND userDevice.project.id= :projectId";
		Query query = entityManager.createQuery(sql)
				.setParameter("userId", userId)
				.setParameter("projectId", projectId);
	
		query.executeUpdate();
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<UserDevice> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<UserDevice> findByUserZoneId(Long userId, Long zoneId) {
		String sql = "SELECT DISTINCT userDevice"
				+ " FROM UserDevice AS userDevice"
				+ " WHERE userDevice.user.id = :userId"
				+ " AND userDevice.device.zone.id = :zoneId"
				+ " ORDER BY userDevice.device.order";
	
		Query query = entityManager.createQuery(sql)
					.setParameter("userId", userId)
					.setParameter("zoneId", zoneId);
		
		return findMany(query);
	}

	@Override
	public void removeByUserZoneId(long userId, long zoneId) {
		String sql = "DELETE FROM UserDevice AS userDevice"
				+ " WHERE userDevice.user.id= :userId"
				+ " AND userDevice.device.zone.id= :zoneId";
		Query query = entityManager.createQuery(sql)
				.setParameter("userId", userId)
				.setParameter("zoneId", zoneId);
	
		query.executeUpdate();
	}

	@Override
	public void updateRoleByUserZone(UserZone userZone) {
		String sql = "UPDATE UserDevice as userDevice SET userDevice.role = :role" 
				+ " WHERE userDevice.device.id IN"
				+ " (SELECT device.id FROM Device AS device WHERE device.zone.id = :zoneId)"
				+ " AND userDevice.user.id = :userId";
		Query query = entityManager.createQuery(sql)
				.setParameter("role", userZone.getRole())
				.setParameter("zoneId", userZone.getZone().getId())
				.setParameter("userId", userZone.getUser().getId());
	
		query.executeUpdate();
		
	}
}
