package net.ionoff.center.server.persistence.service;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.RelayGroup;
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.RelayGroupDto;

@Transactional
public interface IRelayGroupService extends IGenericService<RelayGroup, RelayGroupDto> {

	RelayGroupDto findByRelayId(Long relayId);

	RelayGroupDto addRelayToGroup(Long relayId, RelayDto relayDto);

	RelayGroupDto removeRelayFromGroup(Long relayId, Long relayIdToRemove);
	
}
