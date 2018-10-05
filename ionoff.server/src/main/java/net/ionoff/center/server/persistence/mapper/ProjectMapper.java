package net.ionoff.center.server.persistence.mapper;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.entity.Area;
import net.ionoff.center.server.entity.Project;
import net.ionoff.center.server.entity.Zone;
import net.ionoff.center.shared.dto.ProjectDto;
import net.ionoff.center.shared.dto.ZoneDto;

public class ProjectMapper {

	@Autowired
	private ZoneMapper zoneMapper;

	public Project createProject(ProjectDto projectDto) {
		final Project project = new Project();
		updateProject(project, projectDto);
		return project;
	}

	public Project updateProject(Project project, ProjectDto projectDto) {
		project.setName(projectDto.getName());
		project.setAddress(projectDto.getAddress());
		return project;
	}
	
	public List<ProjectDto> createProjectDtoList(List<Project> projects) {
		final List<ProjectDto> projectDtos = new ArrayList<ProjectDto>();
		for (final Project project : projects) {
			projectDtos.add(createProjectDto(project, false));
		}
		return projectDtos;
	}
	
	public ProjectDto createProjectDto(Project project, boolean includingZones) {
		final ProjectDto projectDto = new ProjectDto();
		projectDto.setId(project.getId());
		projectDto.setName(project.getName());
		projectDto.setAddress(project.getAddress());
		if (project.getActivatedMode() != null) {
			projectDto.setActivatedModeId(project.getActivatedMode().getId());
		}
		if (includingZones) {
			final List<ZoneDto> zoneDtos = new ArrayList<ZoneDto>();
			for (final Area area : project.getAreas()) {
				for (final Zone zone : area.getZones()) {
					zoneDtos.add(zoneMapper.createZoneDto(zone, false));
				}
			}
			projectDto.setZones(zoneDtos);
		}
		return projectDto;
	}


}
