package net.ionoff.center.server.persistence.mapper;

import java.util.ArrayList;
import java.util.List;

import net.ionoff.center.server.entity.Project;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.shared.dto.RelayDriverDto;
import net.ionoff.center.shared.dto.RelayDto;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Component;

@Component
public class RelayDriverMapper {

	private static final Logger LOGGER = Logger.getLogger(RelayDriverMapper.class.getName());

	
	public RelayDriver updateRelayDriver(RelayDriver relayDriver, 
			RelayDriverDto relayDriverDto, Project project) {
		relayDriver.setName(relayDriverDto.getName());
		relayDriver.setIp(relayDriverDto.getIp());
		relayDriver.setPort(relayDriverDto.getPort());
		relayDriver.setKey(relayDriverDto.getKey());
		relayDriver.setProject(project);
		return relayDriver;
	}
	
	public RelayDriver createRelayDriver(RelayDriverDto relayDriverDto, Project project) {
		RelayDriver relayDriver = newRelayDriver(relayDriverDto.getModel());
		updateRelayDriver(relayDriver, relayDriverDto, project);
		return relayDriver;
	}

	private RelayDriver newRelayDriver(String model) {
		Class clazz = RelayDriver.MODELS.get(model);
		if (clazz == null) {
			throw new RuntimeException("No relay driver class map with model " + model);
		}
		else {
			try {
				RelayDriver relayDriver = (RelayDriver) clazz.newInstance();
				return relayDriver;
			} catch (Exception e) {
				LOGGER.error("Error create instanse for class " + clazz.getName());
				throw new RuntimeException("Error create instanse for class " + clazz.getName());
			}
		}
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
		relayDriverDto.setModel(relayDriver.getModel());
		relayDriverDto.setProjectId(relayDriver.getProject().getId());
		relayDriverDto.setIsOnline(relayDriver.isConnected());
		return relayDriverDto;
	}
}
