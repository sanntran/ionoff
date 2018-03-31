package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Scene;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.shared.dto.SceneDto;

@Transactional
public interface ISceneService extends IGenericService<Scene, SceneDto> {
	
	SceneDto finDtoById(Long sceneid);
	
	List<SceneDto> findDtoByProjectId(long projectId);

	List<SceneDto> findDtoByUserZone(User user, long zoneId);

	List<SceneDto> findDtoByUserProject(User user, Long projectId);

}
