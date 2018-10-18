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

import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.ControllerDto;

/**
 * @author Sann Tran
 */
public interface ControllerService extends EntityService<ControllerDto> {

	@PUT
	@Path("controllers/{controllerId}")
	void save(@PathParam("controllerId") Long controllerId, ControllerDto controllerDto,
			MethodCallback<ControllerDto> callback);
	
	@Override
	@DELETE
	@Path("controllers/{controllerId}")
	void delete(@PathParam("controllerId") Long controllerId, MethodCallback<MessageDto> callback);
	
	@GET
	@Path("controllers/{controllerId}")
	void findById(@PathParam("controllerId") Long controllerId, MethodCallback<ControllerDto> callback);
	
	@GET
	@Path("controllers")
	void findByProjectId(@QueryParam("projectId") Long projectId, MethodCallback<List<ControllerDto>> callback);
	
	@Override
	@POST
	@Path("controllers/search")
	void searchByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<List<ControllerDto>> callback);
	
	@Override
	@POST
	@Path("controllers/count")
	void countByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<Long> callback);
}
