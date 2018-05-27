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
import net.ionoff.center.shared.dto.SensorDataDto;

/**
 * @author Sann Tran
 */
public interface SensorDataService extends EntityService<SensorDataDto> {


	@Override
	@POST
	@Path("api/sensordata/count")
	void countByCriteria(QueryCriteriaDto criteriaDto,
                         MethodCallback<Long> callback);

	@Override
	@POST
	@Path("api/sensordata/search")
	void searchByCriteria(QueryCriteriaDto criteriaDto,
						  MethodCallback<List<SensorDataDto>> callback);

	@POST
	@Path("api/sensordata/loadbyday")
	void searchByDay(QueryCriteriaDto criteriaDto,
						   MethodCallback<List<SensorDataDto>> callback);

	@PUT
	@Path("api/sensordata/{id}")
	void save(@PathParam("id") Long id, SensorDataDto dto, MethodCallback<SensorDataDto> callback);
	
	@Override
	@DELETE
	@Path("api/sensordata/{id}")
	void delete(@PathParam("id") Long id, MethodCallback<MessageDto> callback);
	
	@GET
	@Path("api/sensordata/{id}")
	void findById(@PathParam("id") Long id, MethodCallback<SensorDataDto> callback);

}
