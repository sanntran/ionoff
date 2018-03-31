package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.SceneDevice;
import net.ionoff.center.shared.dto.SceneDeviceDto;

@Transactional
public interface ISceneDeviceService extends IGenericService<SceneDevice, SceneDeviceDto> {

	SceneDevice findBySceneIdDeviceId(long sceneId, long deviceId);
	
	SceneDeviceDto findDtoBySceneIdDeviceId(long sceneId, long deviceId);

	List<SceneDeviceDto> findDtoBySceneId(Long sceneId);
}
