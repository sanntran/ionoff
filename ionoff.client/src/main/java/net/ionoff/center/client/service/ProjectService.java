package net.ionoff.center.client.service;

import java.util.List;

import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;

import org.fusesource.restygwt.client.MethodCallback;

import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.ProjectDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;

/**
 * @author Sann Tran
 */
public interface ProjectService extends EntityService<ProjectDto> {

	@PUT
	@Path("api/projects/{projectId}")
	void save(@PathParam("projectId") Long projectId, ProjectDto projectDto, 
			MethodCallback<ProjectDto> callback);
	
	@Override
	@DELETE
	@Path("api/projects/{projectId}")
	void delete(@PathParam("projectId") Long projectId, MethodCallback<MessageDto> callback);

	@GET
	@Path("api/projects/{projectId}")
	void findById(@PathParam("projectId") Long projectId, MethodCallback<ProjectDto> callback);
	
	@GET
	@Path("api/projects")
	void getAll(MethodCallback<List<ProjectDto>> callback);

	@Override
	@POST
	@Path("api/projects/search")
	void searchByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<List<ProjectDto>> callback);

	@Override
	@POST
	@Path("api/projects/count")
	void countByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<Long> callback);
}
