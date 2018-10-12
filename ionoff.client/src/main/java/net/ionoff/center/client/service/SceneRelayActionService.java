package net.ionoff.center.client.service;

import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;

import org.fusesource.restygwt.client.MethodCallback;
import org.fusesource.restygwt.client.RestService;

import net.ionoff.center.shared.dto.SceneRelayActionDto;

/**
 * @author Sann Tran
 */
public interface SceneRelayActionService extends RestService {
	
	@PUT
	@Path("scenerelayactions/{sceneRelayActionId}")
	void save(@PathParam("sceneRelayActionId") Long sceneRelayActionId, 
			SceneRelayActionDto sceneRelayActionDto, MethodCallback<SceneRelayActionDto> callback);
}
