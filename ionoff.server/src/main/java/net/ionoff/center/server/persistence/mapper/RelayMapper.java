package net.ionoff.center.server.persistence.mapper;

import java.util.ArrayList;
import java.util.List;

import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.RelayGroup;
import net.ionoff.center.server.entity.RelayGroupRelay;
import net.ionoff.center.server.util.DateTimeUtil;
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.RelayGroupDto;
import org.springframework.stereotype.Component;

@Component
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
		relay.setAutoRevert(relayDto.getAutoRevert());
		relay.setIsLocked(relayDto.getIsLocked());
		return relay;
	}
	
	public RelayDto createRelayDto(Relay relay) {
		final RelayDto relayDto = new RelayDto();
		relayDto.setId(relay.getId());
		relayDto.setName(relay.getName());
		relayDto.setLabel(relay.getLabel());
		relayDto.setIndex(relay.getIndex() + 1);
		relayDto.setStatus(relay.getStatus());
		relayDto.setIsLocked(relay.getIsLocked());
		relayDto.setAutoRevert(relay.getAutoRevert());
		if (relay.getTime() != null) {
			relayDto.setTime(DateTimeUtil.ddMMHHmmFormatter.format(relay.getTime()));
		}
		relayDto.setDriverId(relay.getDriver().getId());
		relayDto.setDriverName(relay.getDriver().getName());
		if (relay.getDevice() != null) {
			relayDto.setDeviceId(relay.getDevice().getId());
			relayDto.setDeviceName(relay.getDevice().getName());
		}
		return relayDto;
	}
	
	public RelayGroupDto createRelayGroupDto(RelayGroup relayGroup) {
		RelayGroupDto relayGroupDto = new RelayGroupDto();
		relayGroupDto.setId(relayGroup.getId());
		relayGroupDto.setRelays(new ArrayList<>());
		if (relayGroup.getGroupRelays() != null) {
			for (RelayGroupRelay groupRelay : relayGroup.getGroupRelays()) {
				Relay relay = groupRelay.getRelay();
				RelayDto relayDto = new RelayDto();
				relayDto.setId(relay.getId());
				relayDto.setName(relay.getName());
				relayDto.setIsLeader(groupRelay.getIsLeader());
				if (relay.getDevice() != null) {
					relayDto.setDeviceId(relay.getDevice().getId());
					relayDto.setDeviceName(relay.getDevice().getName());
				}
				relayDto.setDriverName(relay.getDriver().getName());
				relayGroupDto.getRelays().add(relayDto);
			}
		}
		return relayGroupDto;
	}
}
