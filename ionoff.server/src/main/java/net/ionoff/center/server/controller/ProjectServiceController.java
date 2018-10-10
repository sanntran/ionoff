package net.ionoff.center.server.controller;

import java.util.List;

import javax.servlet.http.HttpServletRequest;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.exception.ChangeEntityIdException;
import net.ionoff.center.server.exception.DeleteEntityException;
import net.ionoff.center.server.persistence.service.IProjectService;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.ProjectDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;

@RestController
public class ProjectServiceController {

	private final Logger logger = Logger.getLogger(ProjectServiceController.class.getName());

	@Autowired
	private IProjectService projectService;

	@RequestMapping(value = "projects",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public List<ProjectDto> loadAll() {
		
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkProjectPermission(user, null);
		
		return projectService.loadAllDtos();
	}
	
	@RequestMapping(value = "projects/count",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public Long countByCriteria(
			@RequestBody QueryCriteriaDto criteriaDto,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, criteriaDto.getProjectId());
		return projectService.countByCriteria(criteriaDto);
	}
	
	@RequestMapping(value = "projects/search",
			method = RequestMethod.POST,
			produces = "application/json; charset=utf-8")

	public List<ProjectDto> searchByCriteria(@RequestBody QueryCriteriaDto criteriaDto,
			HttpServletRequest request) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, criteriaDto.getProjectId());
		return projectService.searchByCriteria(criteriaDto);
	}
	
	@RequestMapping(value = "projects/{projectId}",
			method = RequestMethod.PUT,
			produces = "application/json; charset=utf-8")

	public ProjectDto insertOrUpdate(@PathVariable("projectId") Long projectId,
			@RequestBody ProjectDto projectDto, HttpServletRequest request) {

		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, null);
		
		if (!projectId.equals(projectDto.getId()) && !projectDto.izNew()) {
			throw new ChangeEntityIdException(projectDto.toString());
		}
		
		if (projectDto.izNew()) {
			logger.info("User " + user.getName() + " inserts project: " + projectDto.toString());
			return projectService.insertDto(user, projectDto);
		}
		else {
			logger.info("User " + user.getName() + " updates project: " + projectDto.toString());
			return projectService.updateDto(user, projectDto);
		}
	}
	
	@RequestMapping(value = "projects/{projectId}",
			method = RequestMethod.DELETE,
			produces = "application/json; charset=utf-8")

	public MessageDto delete(@PathVariable("projectId") Long projectId,
			HttpServletRequest request) throws DeleteEntityException {

		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkAdminPermission(user);
		RequestContextHolder.checkProjectPermission(user, null);
		
		logger.info("User " + user.getName() + " delete project: Project ID: " + projectId);
		projectService.deleteDtoById(user, projectId);
		
		return MessageDto.success(projectId);
	}
	
	@RequestMapping(value = "projects/{projectId}",
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public ProjectDto findById(@PathVariable("projectId") Long projectId) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkProjectPermission(user, projectId);
		final ProjectDto projectDto = projectService.requireDtoById(projectId);
		return projectDto;
	}

 }
