package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.SceneAction;
import net.ionoff.center.shared.dto.SceneActionDto;
import net.ionoff.center.shared.dto.ScenePlayerActionDto;
import net.ionoff.center.shared.dto.SceneRelayActionDto;

@Transactional
public interface ISceneActionService extends IGenericService<SceneAction, SceneActionDto> {
	
	List<SceneActionDto> findDtoBySceneDeviceId(long sceneDeviceId);
	
	List<SceneActionDto> findDtoBySceneIdDeviceId(long sceneId, long deviceId);
	
	ScenePlayerActionDto updateScenePlayerActionDto(ScenePlayerActionDto scenePlayerActionDto);

	SceneRelayActionDto updateSceneRelayActionDto(SceneRelayActionDto sceneRelayActionDto);
	
}
