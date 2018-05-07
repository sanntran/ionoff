package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.entity.Switch;
import net.ionoff.center.shared.dto.RelayDriverDto;

@Transactional
public interface IRelayDriverService extends IGenericService<RelayDriver, RelayDriverDto> {

	List<RelayDriver> findByProjectId(long projectId);

	List<RelayDriver> findByMac(String mac);

	List<RelayDriver> findByIp(String ip);

	List<RelayDriverDto> findDtoByProjectId(Long projectId);
	
	void insertSwitches(RelayDriver relayDriver);
	
	Switch updateSwitch(Switch zwitch);
}
