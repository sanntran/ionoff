package net.ionoff.center.client.service;

import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;

import org.fusesource.restygwt.client.MethodCallback;
import org.fusesource.restygwt.client.RestService;

import net.ionoff.center.shared.dto.ModeSensorDto;

/**
 * @author Sann Tran
 */
public interface ModeSensorService extends RestService {
	
	@PUT
	@Path("api/modesensors/{modeSensorId}")
	void update(@PathParam("modeSensorId") Long modeSensorId, 
			ModeSensorDto modeSensorDto, MethodCallback<ModeSensorDto> callback);
}
