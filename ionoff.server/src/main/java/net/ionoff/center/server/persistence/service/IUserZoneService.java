package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.UserZone;
import net.ionoff.center.shared.dto.UserZoneDto;

@Transactional
public interface IUserZoneService extends IGenericService<UserZone, UserZoneDto> {

	List<UserZone> findByUserProjectId(Long userId, Long projectId);

	List<UserZoneDto> findDtoByUserProject(long userId, long projectId);

	void removeByUserProjectId(long userId, long projectId);
}
