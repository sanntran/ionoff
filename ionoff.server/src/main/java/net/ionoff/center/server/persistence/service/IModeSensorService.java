package net.ionoff.center.server.persistence.service;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeSensor;
import net.ionoff.center.shared.dto.ModeSensorDto;

@Transactional
public interface IModeSensorService extends IGenericService<ModeSensor, ModeSensorDto> {
	
}
