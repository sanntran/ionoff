package net.ionoff.center.server.persistence.dao;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.DashboardDevice;

@Transactional
public interface IDashboardDeviceDao extends IGenericDao<DashboardDevice> {

	DashboardDevice findByDashboardDeviceId(long dashboardId, long deviceId);
}
