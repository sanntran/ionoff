package net.ionoff.center.server.objmapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.persistence.service.IProjectService;
import net.ionoff.center.shared.dto.RelayDriverDto;
import net.ionoff.center.shared.dto.RelayDto;

public class RelayDriverMapper {
	
	@Autowired
	private IProjectService projectService;
	
	@Autowired
	private RelayMapper relayMapper;
	
	public RelayDriver updateRelayDriver(RelayDriver relayDriver, RelayDriverDto relayDriverDto) {
		relayDriver.setName(relayDriverDto.getName());
		relayDriver.setIp(relayDriverDto.getIp());
		relayDriver.setPort(relayDriverDto.getPort());
		relayDriver.setKey(relayDriverDto.getKey());
		relayDriver.setModel(relayDriverDto.getModel().toString());
		relayDriver.setProject(projectService.findById(relayDriverDto.getProjectId()));
		return relayDriver;
	}
	
	public RelayDriver createRelayDriver(RelayDriverDto relayDriverDto) {
		RelayDriver relayDriver = new RelayDriver();
		updateRelayDriver(relayDriver, relayDriverDto);
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
