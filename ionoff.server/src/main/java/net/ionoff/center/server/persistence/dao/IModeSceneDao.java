package net.ionoff.center.server.persistence.dao;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeScene;

@Transactional
public interface IModeSceneDao extends IGenericDao<ModeScene> {
	
}
