package net.ionoff.center.client.service;

import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;

import org.fusesource.restygwt.client.MethodCallback;
import org.fusesource.restygwt.client.RestService;

import net.ionoff.center.shared.dto.ScenePlayerActionDto;

/**
 * @author Sann Tran
 */
public interface ScenePlayerActionService extends RestService {
	
	@PUT
	@Path("sceneplayeractions/{scenePlayerActionId}")
	void save(@PathParam("scenePlayerActionId") Long scenePlayerActionId, 
			ScenePlayerActionDto scenePlayerActionDto, MethodCallback<ScenePlayerActionDto> callback);
}
