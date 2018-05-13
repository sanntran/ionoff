package net.ionoff.center.server.persistence.dao;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Switch;

@Transactional
public interface ISwitchDao extends IGenericDao<Switch> {

	Switch findByDriverId(Long driverId, Integer index);
	
}
