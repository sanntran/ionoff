package net.ionoff.center.server.objmapper;

import java.util.ArrayList;
import java.util.List;

import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.RelayGroup;
import net.ionoff.center.server.util.DateTimeUtil;
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.RelayGroupDto;

public class RelayMapper {

	public List<RelayDto> createRelayDtoList(List<Relay> relays) {
		final List<RelayDto> relayDtos = new ArrayList<RelayDto>();
		for (final Relay relay : relays) {
			relayDtos.add(createRelayDto(relay));
		}
		return relayDtos;
	}
	
	public Relay updateRelay(Relay relay, RelayDto relayDto) {
		relay.setName(relayDto.getName());
		relay.setType(relayDto.getType());
		return relay;
	}
	
	public RelayDto createRelayDto(Relay relay) {
		final RelayDto relayDto = new RelayDto();
		relayDto.setId(relay.getId());
		relayDto.setName(relay.getName());
		relayDto.setLabel(relay.getLabel());
		relayDto.setIndex(relay.getIndex() + 1);
		relayDto.setStatus(relay.getStatus());
		relayDto.setIsLeader(relay.getIsLeader());
		relayDto.setType(relay.getType());
		if (relay.getTime() != null) {
			relayDto.setTime(DateTimeUtil.ddMMHHmmFormatter.format(relay.getTime()));
		}
		relayDto.setControllerId(relay.getController().getId());
		relayDto.setControllerName(relay.getController().getName());
		if (relay.getDevice() != null) {
			relayDto.setDeviceId(relay.getDevice().getId());
			relayDto.setDeviceName(relay.getDevice().getName());
		}
		return relayDto;
	}
	
	public RelayGroupDto createRelayGroupDto(RelayGroup relayGroup) {
		RelayGroupDto relayGroupDto = new RelayGroupDto();
		relayGroupDto.setRelays(new ArrayList<>());
		if (relayGroup.getRelays() != null) {
			for (Relay relay : relayGroup.getRelays()) {
				RelayDto relayDto = new RelayDto();
				relayDto.setId(relay.getId());
				relayDto.setName(relay.getName());
				relayDto.setIsLeader(relay.getIsLeader());
				if (relay.getDevice() != null) {
					relayDto.setDeviceId(relay.getDevice().getId());
					relayDto.setDeviceName(relay.getDevice().getName());
				}
				relayDto.setControllerName(relay.getController().getName());
				relayGroupDto.getRelays().add(relayDto);
			}
		}
		return relayGroupDto;
	}
}
