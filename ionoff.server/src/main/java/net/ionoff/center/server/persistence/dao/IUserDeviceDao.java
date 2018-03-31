package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.UserDevice;
import net.ionoff.center.server.entity.UserZone;

@Transactional
public interface IUserDeviceDao extends IGenericDao<UserDevice> {
	
	List<UserDevice> findByUserZoneId(Long userId, Long zoneId);
	
	List<UserDevice> findByUserProjectId(Long userId, Long projectId);

	void removeByUserZoneId(long userId, long zoneId);
	
	void removeByUserProjectId(long userId, long projectId);

	void updateRoleByUserZone(UserZone userZone);
}
