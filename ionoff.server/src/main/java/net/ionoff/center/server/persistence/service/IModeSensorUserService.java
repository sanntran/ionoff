package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeSensorUser;
import net.ionoff.center.shared.dto.ModeSensorUserDto;

@Transactional
public interface IModeSensorUserService extends IGenericService<ModeSensorUser, ModeSensorUserDto> {

	List<ModeSensorUserDto> findByModeSensorId(Long modeSensorId);
	
}
