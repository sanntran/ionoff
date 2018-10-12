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
	@Path("relays/{relayId}")
	void save(@PathParam("relayId") Long relayId, RelayDto relayDto, MethodCallback<RelayDto> callback);
	
	@Override
	@DELETE
	@Path("relays/{relayId}")
	void delete(@PathParam("relayId") Long relayId, MethodCallback<MessageDto> callback);
	
	@Override
	@POST
	@Path("relays/search")
	void searchByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<List<RelayDto>> callback);
	
	@Override
	@POST
	@Path("relays/count")
	void countByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<Long> callback);
	
	@POST
	@Path("relays/{relayId}/closeopen")
	void closeOpenRelay(@PathParam("relayId") Long relayId, MethodCallback<StatusDto> callback);
	
	@POST
	@Path("relays/{relayId}/open")
	void openRelay(@PathParam("relayId") Long relayId, MethodCallback<StatusDto> callback);
	
	@POST
	@Path("relays/{relayId}/close")
	void closeRelay(@PathParam("relayId") Long relayId, MethodCallback<StatusDto> callback);
	
	@GET
	@Path("relays/{relayId}/groups")
	void getRelayGroups(@PathParam("relayId") Long relayId, MethodCallback<List<RelayGroupDto>> callback);
	
	@POST
	@Path("relays/{relayId}/groups")
	void createRelayGroups(@PathParam("relayId") Long relayId, MethodCallback<RelayGroupDto> methodCallback);
	
	@DELETE
	@Path("relaygroups/{groupId}")
	void deleteGroupById(@PathParam("groupId") Long groupId, MethodCallback<MessageDto> methodCallback);

	@POST
	@Path("relaygroups/{groupId}/relays")
	void addToRelayGroup(@PathParam("groupId") Long groupId, RelayDto relayDto, MethodCallback<RelayGroupDto> callback);
	
	@DELETE
	@Path("relaygroups/{groupId}/relays/{relayId}")
	void removeFromRelayGroup(@PathParam("groupId") Long groupId, @PathParam("relayId") Long relayId,
			MethodCallback<RelayGroupDto> callback);

	@PUT
	@Path("relaygroups/{groupId}/relays/{relayId}/leader")
	void updateLeader(@PathParam("groupId") Long groupId, @PathParam("relayId") Long relayId, 
			@QueryParam("isLeader") Boolean isLeader, MethodCallback<MessageDto> methodCallback);

}
