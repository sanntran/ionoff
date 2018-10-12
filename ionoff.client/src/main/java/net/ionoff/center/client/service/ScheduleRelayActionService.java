package net.ionoff.center.client.service;

import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;

import org.fusesource.restygwt.client.MethodCallback;
import org.fusesource.restygwt.client.RestService;

import net.ionoff.center.shared.dto.ScheduleRelayActionDto;

/**
 * @author Sann Tran
 */
public interface ScheduleRelayActionService extends RestService {
	
	@PUT
	@Path("schedulerelayactions/{scheduleRelayActionId}")
	void save(@PathParam("scheduleRelayActionId") Long scheduleRelayActionId, 
			ScheduleRelayActionDto scheduleRelayActionDto, MethodCallback<ScheduleRelayActionDto> callback);
}
