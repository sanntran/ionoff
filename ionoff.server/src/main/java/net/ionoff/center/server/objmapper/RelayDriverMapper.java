package net.ionoff.center.server.objmapper;

import java.util.ArrayList;
import java.util.List;

import net.ionoff.center.server.entity.Project;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.shared.dto.RelayDriverDto;
import net.ionoff.center.shared.dto.RelayDto;

public class RelayDriverMapper {
	
	public RelayDriver updateRelayDriver(RelayDriver relayDriver, 
			RelayDriverDto relayDriverDto, Project project) {
		relayDriver.setName(relayDriverDto.getName());
		relayDriver.setIp(relayDriverDto.getIp());
		relayDriver.setPort(relayDriverDto.getPort());
		relayDriver.setKey(relayDriverDto.getKey());
		relayDriver.setModel(relayDriverDto.getModel().toString());
		relayDriver.setProject(project);
		return relayDriver;
	}
	
	public RelayDriver createRelayDriver(RelayDriverDto relayDriverDto, Project project) {
		RelayDriver relayDriver = new RelayDriver();
		updateRelayDriver(relayDriver, relayDriverDto, project);
		return relayDriver;
	}

	public List<RelayDriverDto> createRelayDriverDtoList(List<RelayDriver> relayDrivers) {
		final List<RelayDriverDto> relayDriverDtos = new ArrayList<RelayDriverDto>();
		for (final RelayDriver relayDriver : relayDrivers) {
			RelayDriverDto relayDriverDto = createRelayDriverDto(relayDriver);
			relayDriverDto.setRelays(new ArrayList<>());
			for (Relay relay : relayDriver.getRelays()) {
				RelayDto relayDto = new RelayDto();
				relayDto.setId(relay.getId());
				relayDto.setName(relay.getName());
				if (relay.getDevice() != null) {
					relayDto.setDeviceId(relay.getDevice().getId());
					relayDto.setDeviceName(relay.getDevice().getName());
				}
				relayDriverDto.getRelays().add(relayDto);
			}
			relayDriverDtos.add(relayDriverDto);
			
		}
		return relayDriverDtos;
	}
	
	public RelayDriverDto createRelayDriverDto(RelayDriver relayDriver) {
		final RelayDriverDto relayDriverDto = new RelayDriverDto();
		relayDriverDto.setId(relayDriver.getId());
		relayDriverDto.setName(relayDriver.getName());
		relayDriverDto.setIp(relayDriver.getIp());
		relayDriverDto.setPort(relayDriver.getPort());
		relayDriverDto.setKey(relayDriver.getKey());
		relayDriverDto.setModel(relayDriver.getModelObj());
		relayDriverDto.setProjectId(relayDriver.getProject().getId());
		relayDriverDto.setIsOnline(relayDriver.isConnected());
		return relayDriverDto;
	}
}
