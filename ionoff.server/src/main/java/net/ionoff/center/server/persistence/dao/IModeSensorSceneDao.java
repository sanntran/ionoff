package net.ionoff.center.server.persistence.dao;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeSensorScene;

@Transactional
public interface IModeSensorSceneDao extends IGenericDao<ModeSensorScene> {
	
}
