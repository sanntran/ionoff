package net.ionoff.center.server.persistence.dao;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.RelayGroup;

@Transactional
public interface IRelayGroupDao extends IGenericDao<RelayGroup> {
	
}
