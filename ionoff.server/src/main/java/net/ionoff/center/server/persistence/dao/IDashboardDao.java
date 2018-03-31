package net.ionoff.center.server.persistence.dao;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Dashboard;

@Transactional
public interface IDashboardDao extends IGenericDao<Dashboard> {

	Dashboard findByUserZoneId(long userId, long zoneId);

	Dashboard findByUserProjectId(long userId, long projectId);

	void removeByUserProjectId(long userId, long projectId);

}
