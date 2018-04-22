package net.ionoff.center.client.service;

import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.SensorDataDto;
import net.ionoff.center.shared.dto.SensorDto;
import org.fusesource.restygwt.client.MethodCallback;

import javax.ws.rs.*;
import java.util.List;

/**
 * @author Sann Tran
 */
public interface SensorDataService extends EntityService<SensorDataDto> {

	@Override
	@POST
	@Path("api/sensordata/search")
	void searchByCriteria(QueryCriteriaDto criteriaDto,
                          MethodCallback<List<SensorDataDto>> callback);

	@Override
	@POST
	@Path("api/sensordata/count")
	void countByCriteria(QueryCriteriaDto criteriaDto,
                         MethodCallback<Long> callback);
	
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
