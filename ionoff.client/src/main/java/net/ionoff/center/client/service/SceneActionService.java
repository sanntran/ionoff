package net.ionoff.center.client.service;

import java.util.List;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.QueryParam;

import org.fusesource.restygwt.client.MethodCallback;
import org.fusesource.restygwt.client.RestService;

import net.ionoff.center.shared.dto.SceneActionDto;

/**
 * @author Sann Tran
 */
public interface SceneActionService extends RestService {
	
	@GET
	@Path("sceneactions")
	void findBySceneDevice(
			@QueryParam("sceneDeviceId") Long sceneDeviceId,
			MethodCallback<List<SceneActionDto>> callback);
	
}
