package net.ionoff.center.client.service;

import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;

import org.fusesource.restygwt.client.MethodCallback;
import org.fusesource.restygwt.client.RestService;

import net.ionoff.center.shared.dto.ModeSensorSceneDto;

/**
 * @author Sann Tran
 */
public interface ModeSensorSceneService extends RestService {

	@PUT
	@Path("modesensorscenes/{modeSensorSceneId}")
	void update(@PathParam("modeSensorSceneId") Long modeSensorSceneId,
			ModeSensorSceneDto modeSensorSceneDto, MethodCallback<ModeSensorSceneDto> callback);

	@GET
	@Path("modesensors/{modeSensorId}/modesensorscenes")
	void findByModeSensorId(@PathParam("modeSensorId") Long modeSensorId,
			MethodCallback<List<ModeSensorSceneDto>> callback);
}
