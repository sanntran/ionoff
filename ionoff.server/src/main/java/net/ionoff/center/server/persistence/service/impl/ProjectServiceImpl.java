package net.ionoff.center.server.persistence.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import net.ionoff.center.server.entity.Project;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.entity.UserProject;
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.locale.Constants;
import net.ionoff.center.server.locale.Messages;
import net.ionoff.center.server.persistence.mapper.ProjectMapper;
import net.ionoff.center.server.persistence.dao.IProjectDao;
import net.ionoff.center.server.persistence.service.IProjectService;
import net.ionoff.center.server.persistence.service.IUserProjectService;
import net.ionoff.center.server.persistence.service.IUserService;
import net.ionoff.center.shared.dto.ProjectDto;

@Service
@Transactional
public class ProjectServiceImpl extends AbstractGenericService<Project, ProjectDto> implements IProjectService {

	private IProjectDao projectDao;
	
	@Autowired
	private ProjectMapper projectMapper;
	
	@Autowired
	private IUserService userService;
	
	@Autowired
	private IUserProjectService userProjectService;

	@Autowired
	public ProjectServiceImpl(IProjectDao projectDao) {
		this.projectDao = projectDao;
	}

	@Override
	protected IProjectDao getDao() {
		return projectDao;
	}
	
	@Override
	public Project insert(Project entity) {
		super.insert(entity);
		insertUserProjects(entity);
		return entity;
	}
	
	private void insertUserProjects(Project project) {
		for (User user : userService.loadAll()) {
			UserProject userProject = new UserProject();
			userProject.setUser(user);
			userProject.setProject(project);
			userProject.setRole(false);
			userProjectService.insert(userProject);
		}
	}

	@Override
	public ProjectDto insertDto(User user, ProjectDto dto) {
		final Project project = projectMapper.createProject(dto);
		insert(project);
		return projectMapper.createProjectDto(project, false);
	}

	@Override
	public ProjectDto updateDto(User user, ProjectDto dto) {
		final Project project = requireById(dto.getId());
		projectMapper.updateProject(project, dto);
		update(project);
		return projectMapper.createProjectDto(project, false);
	}

	@Override
	public ProjectDto requireDtoById(long id) {
		return projectMapper.createProjectDto(requireById(id), false);
	}

	@Override
	public List<ProjectDto> loadAllDtos() {
		List<Project> projects = loadAll();
		return projectMapper.createProjectDtoList(projects);
	}
	
	@Override
	public void deleteDtoById(User user, long entityId) {
		
		Project project = requireById(entityId);
		
		final long projectCount = loadAll().size();
		if (projectCount < 2) {
			throw new DeleteEntityException(Messages.get(user.getLanguage()).errorDeleteSingleEntity());
		}
		if (project.getAreas() != null && !project.getAreas().isEmpty()) {
			final String entity = Constants.get(user.getLanguage()).project();
			final String usedByEntity = Constants.get(user.getLanguage()).area();
			String message = Messages.get(user.getLanguage())
					.errorDeleteNotEmptyEntity(entity, usedByEntity);
			throw new DeleteEntityException(message);
		}
		delete(project);
	}

	@Override
	protected List<ProjectDto> createDtoList(List<Project> entities) {
		return projectMapper.createProjectDtoList(entities);
	}

	@Override
	public List<ProjectDto> findDtoByUserId(Long userId) {
		User user = userService.requireById(userId);
		List<Project> projects = projectDao.findByUserId(userId);
		return createDtoList(projects);
	}
}
