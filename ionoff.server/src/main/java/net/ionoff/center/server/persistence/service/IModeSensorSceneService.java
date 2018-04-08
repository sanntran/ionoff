package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeSensorScene;
import net.ionoff.center.shared.dto.ModeSensorSceneDto;

@Transactional
public interface IModeSensorSceneService extends IGenericService<ModeSensorScene, ModeSensorSceneDto> {

	List<ModeSensorSceneDto> findByModeSensorId(Long modeSensorId);
	
}
