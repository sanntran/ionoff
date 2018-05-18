package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.RelayGroup;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.RelayGroupDto;

@Transactional
public interface IRelayGroupService extends IGenericService<RelayGroup, RelayGroupDto> {

	List<RelayGroupDto> findByRelayId(Long relayId);

	RelayGroupDto addRelayToGroup(Long groupId, RelayDto relayDto);

	RelayGroupDto removeRelayFromGroup(Long relayId, Long relayIdToRemove);

	RelayGroupDto createRelayGroup(User user, Long relayId);

	void updateRelayLeader(User user, Long groupId, Long relayId, Boolean isLeader);
}
