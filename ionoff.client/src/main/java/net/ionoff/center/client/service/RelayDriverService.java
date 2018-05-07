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
import net.ionoff.center.shared.dto.RelayDriverDto;

/**
 * @author Sann Tran
 */
public interface RelayDriverService extends EntityService<RelayDriverDto> {

	@PUT
	@Path("api/relaydrivers/{relayDriverId}")
	void save(@PathParam("relayDriverId") Long relayDriverId, RelayDriverDto relayDriverDto,
			MethodCallback<RelayDriverDto> callback);
	
	@Override
	@DELETE
	@Path("api/relaydrivers/{relayDriverId}")
	void delete(@PathParam("relayDriverId") Long relayDriverId, MethodCallback<MessageDto> callback);
	
	@GET
	@Path("api/relaydrivers/{relayDriverId}")
	void findById(@PathParam("relayDriverId") Long relayDriverId, MethodCallback<RelayDriverDto> callback);
	
	@GET
	@Path("api/relaydrivers")
	void findByProjectId(@QueryParam("projectId") Long projectId, MethodCallback<List<RelayDriverDto>> callback);
	
	@Override
	@POST
	@Path("api/relaydrivers/search")
	void searchByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<List<RelayDriverDto>> callback);
	
	@Override
	@POST
	@Path("api/relaydrivers/count")
	void countByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<Long> callback);
}
