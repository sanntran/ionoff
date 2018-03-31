package net.ionoff.center.server.persistence.dao;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.DashboardScene;

@Transactional
public interface IDashboardSceneDao extends IGenericDao<DashboardScene> {

	DashboardScene findByDashboardSceneId(long dashboardId, long sceneId);
}
