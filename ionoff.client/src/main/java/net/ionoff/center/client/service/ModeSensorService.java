package net.ionoff.center.client.service;

import java.util.List;

import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.QueryParam;

import org.fusesource.restygwt.client.MethodCallback;
import org.fusesource.restygwt.client.RestService;

import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.ModeSensorDto;

/**
 * @author Sann Tran
 */
public interface ModeSensorService extends RestService {

	@PUT
	@Path("api/modesensors/{modeSensorId}")
	void save(@PathParam("modeSensorId") Long modeSensorId, 
			ModeSensorDto modeSensorDto, MethodCallback<ModeSensorDto> callback);
	
	@DELETE
	@Path("api/modesensors/{modeSensorId}")
	void delete(@PathParam("modeSensorId") Long modeSensorId, 
										MethodCallback<MessageDto> callback);
	
	@GET
	@Path("api/modesensors")
	void findBySensorId(@QueryParam("sensorId") Long sensorId, 
							MethodCallback<List<ModeSensorDto>> methodCallback);
}
