package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.ModeScene;
import net.ionoff.center.shared.dto.ModeSceneDto;

@Transactional
public interface IModeSceneService extends IGenericService<ModeScene, ModeSceneDto> {

	List<ModeSceneDto> findByModeId(Long modeId);
	
}
