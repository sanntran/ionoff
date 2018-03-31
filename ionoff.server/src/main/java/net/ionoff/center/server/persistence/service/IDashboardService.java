package net.ionoff.center.server.persistence.service;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Dashboard;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.shared.dto.DashboardDto;

@Transactional
public interface IDashboardService extends IGenericService<Dashboard, DashboardDto> {
	
	Dashboard findByUserZoneId(User user, long zoneId);
	
	Dashboard findByUserProjectId(User user, long projectId);
	
	DashboardDto findDtoByUserZoneId(User user, long zoneId);
	
	DashboardDto findDtoByUserProjectId(User user, long projectId);

	void removeByUserProject(User user, long projectId);

	void addDeviceToZoneDashboard(User user, Long deviceId);

	void addDeviceToProjectDashboard(User user, Long deviceId);

	void removeDeviceFromZoneDashboard(User user, Long deviceId);

	void removeDeviceFromProjectDashboard(User user, Long deviceId);

	void addSceneToZoneDashboard(User user, Long sceneId);

	void removeSceneFromZoneDashboard(User user, Long sceneId);

	void addSceneToProjectDashboard(User user, Long sceneId);

	void removeSceneFromProjectDashboard(User user, Long sceneId);
	
}
