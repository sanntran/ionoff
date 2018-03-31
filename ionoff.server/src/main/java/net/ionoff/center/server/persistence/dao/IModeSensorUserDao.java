package net.ionoff.center.server.persistence.dao;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeSensorUser;

@Transactional
public interface IModeSensorUserDao extends IGenericDao<ModeSensorUser> {

	void removeByUserProjectId(long userId, long projectId);
	
}
