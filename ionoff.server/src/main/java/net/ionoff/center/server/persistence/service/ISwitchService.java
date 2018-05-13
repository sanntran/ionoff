package net.ionoff.center.server.persistence.service;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Switch;
import net.ionoff.center.shared.dto.SwitchDto;

@Transactional
public interface ISwitchService extends IGenericService<Switch, SwitchDto> {
	
	Switch findByDriverId(Long driverId, Integer index);
}
