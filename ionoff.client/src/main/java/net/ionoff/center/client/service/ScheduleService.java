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
import net.ionoff.center.shared.dto.ScheduleDto;

/**
 * @author Sann Tran
 */
public interface ScheduleService extends EntityService<ScheduleDto> {
	
	@Override
	@POST
	@Path("schedules/search")
	void searchByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<List<ScheduleDto>> callback);
	
	@Override
	@POST
	@Path("schedules/count")
	void countByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<Long> callback);
	
	@PUT
	@Path("schedules/{scheduleId}")
	void save(@PathParam("scheduleId") Long scheduleId, ScheduleDto scheduleDto, MethodCallback<ScheduleDto> callback);
	
	@Override
	@DELETE
	@Path("schedules/{scheduleId}")
	void delete(@PathParam("scheduleId") Long scheduleId, MethodCallback<MessageDto> callback);
	
	@GET
	@Path("schedules/{scheduleId}")
	void findById(@PathParam("scheduleId") Long scheduleId, MethodCallback<ScheduleDto> callback);
	
	@GET
	@Path("schedules")
	void findByProjectId(@QueryParam("projectId") Long projectId, MethodCallback<List<ScheduleDto>> callback);
}
