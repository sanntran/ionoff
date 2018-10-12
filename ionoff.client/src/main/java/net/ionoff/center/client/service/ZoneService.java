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
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.ZoneDto;

/**
 * @author Sann Tran
 */
public interface ZoneService extends EntityService<ZoneDto> {
	
	@Override
	@POST
	@Path("zones/search")
	void searchByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<List<ZoneDto>> callback);
	
	@Override
	@POST
	@Path("zones/count")
	void countByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<Long> callback);
	
	@PUT
	@Path("zones/{zoneId}")
	void save(@PathParam("zoneId") Long zoneId, ZoneDto zoneDto, MethodCallback<ZoneDto> callback);
	
	@Override
	@DELETE
	@Path("zones/{zoneId}")
	void delete(@PathParam("zoneId") Long zoneId, MethodCallback<MessageDto> callback);
	
	@GET
	@Path("zones/{zoneId}")
	void findById(@PathParam("zoneId") Long zoneId, MethodCallback<ZoneDto> callback);

	@GET
	@Path("projects/{projectId}/zones")
	void findByProjectId(@PathParam("projectId") Long projectId, MethodCallback<List<ZoneDto>> callback);
}
