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
import net.ionoff.center.shared.dto.SensorDto;

/**
 * @author Sann Tran
 */
public interface SensorService extends EntityService<SensorDto> {

	@Override
	@POST
	@Path("api/sensors/count")
	void countByCriteria(QueryCriteriaDto criteriaDto,
						 MethodCallback<Long> callback);

	@Override
	@POST
	@Path("api/sensors/search")
	void searchByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<List<SensorDto>> callback);

	@PUT
	@Path("api/sensors/{sensorId}")
	void save(@PathParam("sensorId") Long sensorId, SensorDto sensorDto, MethodCallback<SensorDto> callback);
	
	@Override
	@DELETE
	@Path("api/sensors/{sensorId}")
	void delete(@PathParam("sensorId") Long sensorId, MethodCallback<MessageDto> callback);
	
	@GET
	@Path("api/sensors/{sensorId}")
	void findById(@PathParam("sensorId") Long sensorId, MethodCallback<SensorDto> callback);
	
	@GET
	@Path("api/sensors")
	void findByProjectId(@QueryParam("projectId") Long projectId, MethodCallback<List<SensorDto>> callback);
}
