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
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.RelayGroupDto;
import net.ionoff.center.shared.dto.StatusDto;

/**
 * @author Sann Tran
 */
public interface RelayService extends EntityService<RelayDto> {

	@PUT
	@Path("api/relays/{relayId}")
	void save(@PathParam("relayId") Long relayId, RelayDto relayDto, MethodCallback<RelayDto> callback);
	
	@Override
	@DELETE
	@Path("api/relays/{relayId}")
	void delete(@PathParam("relayId") Long relayId, MethodCallback<MessageDto> callback);
	
	@Override
	@POST
	@Path("api/relays/search")
	void searchByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<List<RelayDto>> callback);
	
	@Override
	@POST
	@Path("api/relays/count")
	void countByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<Long> callback);
	
	@POST
	@Path("api/relays/{relayId}/closeopen")
	void closeOpenRelay(@PathParam("relayId") Long relayId, MethodCallback<StatusDto> callback);
	
	@POST
	@Path("api/relays/{relayId}/open")
	void openRelay(@PathParam("relayId") Long relayId, MethodCallback<StatusDto> callback);
	
	@POST
	@Path("api/relays/{relayId}/close")
	void closeRelay(@PathParam("relayId") Long relayId, MethodCallback<StatusDto> callback);
	
	@GET
	@Path("api/relays/{relayId}/group")
	void getRelayGroup(@PathParam("relayId") Long relayId, MethodCallback<RelayGroupDto> callback);
	
	@POST
	@Path("api/relays/{relayId}/group")
	void addToRelayGroup(@PathParam("relayId") Long relayId, RelayDto relayDto, MethodCallback<RelayGroupDto> callback);
	
	@DELETE
	@Path("api/relays/{relayId}/group")
	void removeFromRelayGroup(@PathParam("relayId") Long relayId, @QueryParam("relayIdToRemove") Long relayIdToRemove, MethodCallback<RelayGroupDto> callback);
}
