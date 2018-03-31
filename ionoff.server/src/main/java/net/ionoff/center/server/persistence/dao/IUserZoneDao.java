package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.UserZone;

@Transactional
public interface IUserZoneDao extends IGenericDao<UserZone> {
	
	List<UserZone> findByProjectId(Long userId, Long projectId);

	void removeByUserProjectId(long userId, long projectId);

	UserZone findByUserZone(long userId, long zoneId);
}
