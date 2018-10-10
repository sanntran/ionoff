package net.ionoff.center.server.persistence.dao.impl;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.SessionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.QueryCriteria;
import net.ionoff.center.server.entity.SceneDevice;
import net.ionoff.center.server.persistence.dao.ISceneDeviceDao;

@Repository
@Transactional
public class SceneDeviceDaoImpl extends AbstractGenericDao<SceneDevice> implements ISceneDeviceDao {

	@Autowired
	public SceneDeviceDaoImpl(SessionFactory sessionFactory) {
		super(sessionFactory);
		setClass(SceneDevice.class);
	}

	@Override
	public SceneDevice findBySceneIdDeviceId(long sceneId, long deviceId) {
		String sql = "select distinct sceneDevice"
				+ " from SceneDevice as sceneDevice"
				+ " where sceneDevice.scene.id = :sceneId"
				+ " and sceneDevice.device.id = :deviceId";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("sceneId", sceneId)
				.setParameter("deviceId", deviceId);
		return getFirst(findMany(query));
	}

	@Override
	public List<SceneDevice> findByDeviceId(long deviceId) {
		String sql = "select distinct sceneDevice"
				+ " from SceneDevice as sceneDevice"
				+ " where sceneDevice.device.id = :deviceId";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("deviceId", deviceId);
		return findMany(query);
	}

	@Override
	public long countByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<SceneDevice> findByCriteria(QueryCriteria criteria) {
		throw new UnsupportedOperationException();
	}

	@Override
	public List<SceneDevice> findBySceneId(long sceneId) {
		String sql = "select distinct sceneDevice"
				+ " from SceneDevice as sceneDevice"
				+ " where sceneDevice.scene.id = :sceneId";
		Query query = getCurrentSession().createQuery(sql)
				.setParameter("sceneId", sceneId);
		return findMany(query);
	}
}
