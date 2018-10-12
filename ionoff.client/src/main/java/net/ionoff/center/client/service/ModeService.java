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
import net.ionoff.center.shared.dto.ModeDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;

public interface ModeService extends EntityService<ModeDto> {

	@PUT
	@Path("modes/{modeId}")
	void save(@PathParam("modeId") Long modeId, ModeDto modeDto, MethodCallback<ModeDto> callback);
	
	@Override
	@DELETE
	@Path("modes/{modeId}")
	void delete(@PathParam("modeId") Long modeId, MethodCallback<MessageDto> callback);
	
	@Override
	@POST
	@Path("modes/search")
	void searchByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<List<ModeDto>> callback);
	
	@Override
	@POST
	@Path("modes/count")
	void countByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<Long> callback);
	
	@GET
	@Path("modes/{modeId}")
	void findById(@PathParam("modeId") Long modeId, MethodCallback<ModeDto> callback);
	
	@GET
	@Path("modes")
	void findByProjectId(@QueryParam("projectId") Long projectId, MethodCallback<List<ModeDto>> callback);
	
	@GET
	@Path("modes/activated")
	void findByActivated(@QueryParam("projectId") Long projectId, MethodCallback<ModeDto> methodCallback);
	
	@POST
	@Path("modes/{modeId}/activate")
	void activateById(@PathParam("modeId") Long modeId, MethodCallback<MessageDto> methodCallback);
}
