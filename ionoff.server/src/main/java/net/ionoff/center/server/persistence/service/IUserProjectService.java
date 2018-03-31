package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.UserProject;
import net.ionoff.center.shared.dto.UserProjectDto;

@Transactional
public interface IUserProjectService extends IGenericService<UserProject, UserProjectDto> {

	UserProject updateRole(UserProject userProject);
	
	List<UserProject> findByUserId(Long userId);

	List<UserProjectDto> findDtoByUserId(Long userId);
}
