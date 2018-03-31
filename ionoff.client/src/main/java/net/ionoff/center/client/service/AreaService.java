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

import net.ionoff.center.shared.dto.AreaDto;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;

/**
 * @author Sann Tran
 */
public interface AreaService extends EntityService<AreaDto> {

	@PUT
	@Path("api/areas/{areaId}")
	void save(@PathParam("areaId") Long areaId, AreaDto areaDto, MethodCallback<AreaDto> callback);
	
	@Override
	@DELETE
	@Path("api/areas/{areaId}")
	void delete(@PathParam("areaId") Long areaId, MethodCallback<MessageDto> callback);
	
	@GET
	@Path("api/areas/{areaId}")
	void findById(@PathParam("areaId") Long areaId, MethodCallback<AreaDto> callback);
	
	@Override
	@POST
	@Path("api/areas/search")
	void searchByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<List<AreaDto>> callback);
	
	@Override
	@POST
	@Path("api/areas/count")
	void countByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<Long> callback);

	@GET
	@Path("api/areas")
	void findByProjectId(@QueryParam("projectId") Long projectId, 
			@QueryParam("includingZone") boolean includingZone,
			@QueryParam("includingDevice") boolean includingDevice,
			MethodCallback<List<AreaDto>> callback);
	
}
