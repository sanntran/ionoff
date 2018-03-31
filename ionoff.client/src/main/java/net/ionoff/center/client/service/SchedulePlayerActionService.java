package net.ionoff.center.client.service;

import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;

import org.fusesource.restygwt.client.MethodCallback;
import org.fusesource.restygwt.client.RestService;

import net.ionoff.center.shared.dto.SchedulePlayerActionDto;

/**
 * @author Sann Tran
 */
public interface SchedulePlayerActionService extends RestService {
	
	@PUT
	@Path("api/scheduleplayeractions/{schedulePlayerActionId}")
	void save(@PathParam("schedulePlayerActionId") Long schedulePlayerActionId, 
			SchedulePlayerActionDto schedulePlayerActionDto, MethodCallback<SchedulePlayerActionDto> callback);
}
