package net.ionoff.center.server.persistence.dao;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.SceneAction;

@Transactional
public interface ISceneActionDao extends IGenericDao<SceneAction> {
	void deleteByRelayId(long relayId);
}
