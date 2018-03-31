package net.ionoff.center.client.service;

import java.util.List;

import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.QueryParam;

import org.fusesource.restygwt.client.MethodCallback;

import net.ionoff.center.shared.dto.ControllerDto;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;

/**
 * @author Sann Tran
 */
public interface ControllerService extends EntityService<ControllerDto> {

	@PUT
	@Path("api/controllers/{controllerId}")
	void save(@PathParam("controllerId") Long controllerId, ControllerDto controllerDto,
			MethodCallback<ControllerDto> callback);
	
	@Override
	@DELETE
	@Path("api/controllers/{controllerId}")
	void delete(@PathParam("controllerId") Long controllerId, MethodCallback<MessageDto> callback);
	
	@GET
	@Path("api/controllers/{controllerId}")
	void findById(@PathParam("controllerId") Long controllerId, MethodCallback<ControllerDto> callback);
	
	@GET
	@Path("api/controllers")
	void findByProjectId(@QueryParam("projectId") Long projectId, MethodCallback<List<ControllerDto>> callback);
	
	@Override
	@POST
	@Path("api/controllers/search")
	void searchByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<List<ControllerDto>> callback);
	
	@Override
	@POST
	@Path("api/controllers/count")
	void countByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<Long> callback);
}
