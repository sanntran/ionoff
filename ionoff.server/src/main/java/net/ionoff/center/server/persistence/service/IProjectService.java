package net.ionoff.center.server.persistence.service;

import java.util.List;

import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Project;
import net.ionoff.center.shared.dto.ProjectDto;

@Transactional
public interface IProjectService extends IGenericService<Project, ProjectDto> {

	List<ProjectDto> loadAllDtos();

	List<ProjectDto> findDtoByUserId(Long userId);

}
