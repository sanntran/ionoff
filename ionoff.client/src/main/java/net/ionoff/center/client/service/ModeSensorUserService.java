package net.ionoff.center.client.service;

import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;

import org.fusesource.restygwt.client.MethodCallback;
import org.fusesource.restygwt.client.RestService;

import net.ionoff.center.shared.dto.ModeSensorUserDto;

/**
 * @author Sann Tran
 */
public interface ModeSensorUserService extends RestService {

	@PUT
	@Path("modesensorusers/{modeSensorUserId}")
	void update(@PathParam("modeSensorUserId") Long modeSensorUserId,
			ModeSensorUserDto modeSensorUserDto,
			MethodCallback<ModeSensorUserDto> callback);

	@GET
	@Path("modesensors/{modeSensorId}/modesensorusers")
	void findByModeSensorId(@PathParam("modeSensorId") Long modeSensorId,
			MethodCallback<List<ModeSensorUserDto>> callback);


}
