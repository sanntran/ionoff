package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.UserScene;
import net.ionoff.center.shared.dto.UserSceneDto;

@Transactional
public interface IUserSceneService extends IGenericService<UserScene, UserSceneDto> {

	List<UserScene> findByUserProject(Long userId, Long projectId);

	List<UserSceneDto> findDtoByUserProject(long userId, long projectId);
	
	List<UserScene> findByUserZone(Long userId, Long zoneId);

	List<UserSceneDto> findDtoByUserZone(long userId, long zoneId);
}
