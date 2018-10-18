package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.shared.dto.RelayDto;

@Transactional
public interface IRelayService extends IGenericService<Relay, RelayDto> {
	
	List<RelayDto> findDtoByProjectId(long projectId);
	
	List<RelayDto> findDtoByControllerId(long controllerId);
	
	List<RelayDto> findDtoByDeviceId(long deviceId);

	Relay update(Relay relay, Device device);

	Relay update(Relay relay, boolean status);

}
