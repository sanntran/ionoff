package net.ionoff.center.server.persistence.dao;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.RelayGroupRelay;

@Transactional
public interface IRelayGroupRelayDao extends IGenericDao<RelayGroupRelay> {

	RelayGroupRelay findByRelayIdGroupId(Long relayId, Long groupId);
	
}
