package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.UserScene;
import net.ionoff.center.server.entity.UserZone;

@Transactional
public interface IUserSceneDao extends IGenericDao<UserScene> {
	
	List<UserScene> findByZoneId(Long userId, Long zoneId);
	
	List<UserScene> findByProjectId(Long userId, Long projectId);

	void removeByUserZone(long userId, long zoneId);
	
	void removeByUserProjectId(long userId, long projectId);

	void updateRoleByUserZone(UserZone userZone);
}
