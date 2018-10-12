package net.ionoff.center.client.service;

import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;

import org.fusesource.restygwt.client.MethodCallback;
import org.fusesource.restygwt.client.RestService;

import net.ionoff.center.shared.dto.ModeSceneDto;

/**
 * @author Sann Tran
 */
public interface ModeSceneService extends RestService {
	
	@PUT
	@Path("modescenes/{modeSceneId}")
	void save(@PathParam("modeSceneId") Long modeSceneId, 
			ModeSceneDto modeSceneDto, MethodCallback<ModeSceneDto> callback);
}
