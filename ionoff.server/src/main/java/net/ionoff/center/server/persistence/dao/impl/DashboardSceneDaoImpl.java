package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import javax.persistence.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.DashboardScene;
import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.persistence.dao.IDashboardSceneDao;

@Repository
@Transactional
public class DashboardSceneDaoImpl extends AbstractGenericDao<DashboardScene> implements IDashboardSceneDao {

	public DashboardSceneDaoImpl() {
		super();
		setClass(DashboardScene.class);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<DashboardScene> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public DashboardScene findByDashboardSceneId(long dashboardId, long sceneId) {
		String sql = "select distinct dashboardScene"
				+ " from DashboardScene as dashboardScene"
				+ " where dashboardScene.dashboard.id = :dashboardId"
				+ " and dashboardScene.scene.id = :sceneId";
		Query query = entityManager.createQuery(sql)
				.setParameter("dashboardId", dashboardId)
				.setParameter("sceneId", sceneId);
		return getFirst(findMany(query));
	}

}
