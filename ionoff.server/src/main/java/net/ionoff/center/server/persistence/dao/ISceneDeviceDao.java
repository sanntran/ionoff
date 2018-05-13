package net.ionoff.center.server.persistence.dao;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.SceneDevice;

@Transactional
public interface ISceneDeviceDao extends IGenericDao<SceneDevice> {

	SceneDevice findBySceneIdDeviceId(long sceneId, long deviceId);

	List<SceneDevice> findByDeviceId(long deviceId);

	List<SceneDevice> findBySceneId(long sceneId);
}
