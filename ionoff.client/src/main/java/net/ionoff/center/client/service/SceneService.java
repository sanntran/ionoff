package net.ionoff.center.client.service;

import java.util.List;
import java.util.Map;

import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.QueryParam;

import org.fusesource.restygwt.client.MethodCallback;

import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.SceneDeviceDto;
import net.ionoff.center.shared.dto.SceneDto;

/**
 * @author Sann Tran
 */
public interface SceneService extends EntityService<SceneDto> {
	
	@Override
	@POST
	@Path("api/scenes/search")
	void searchByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<List<SceneDto>> callback);
	
	@Override
	@POST
	@Path("api/scenes/count")
	void countByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<Long> callback);
	
	@PUT
	@Path("api/scenes/{sceneId}")
	void save(@PathParam("sceneId") Long sceneId, SceneDto sceneDto, MethodCallback<SceneDto> callback);
	
	@Override
	@DELETE
	@Path("api/scenes/{sceneId}")
	void delete(@PathParam("sceneId") Long sceneId, MethodCallback<MessageDto> callback);
	
	@GET
	@Path("api/scenes/{sceneId}")
	void findById(@PathParam("sceneId") Long sceneId, MethodCallback<SceneDto> callback);
	
	@GET
	@Path("api/scenes")
	void findByProjectId(@QueryParam("projectId") Long projectId, MethodCallback<List<SceneDto>> callback);
	
	@GET
	@Path("api/scenes")
	void findByZoneId(@QueryParam("zoneId") Long zoneId, MethodCallback<List<SceneDto>> methodCallback);
	
	@POST
	@Path("api/scenes/{sceneId}/play")
	void playById(@PathParam("sceneId") Long sceneId, MethodCallback<Map<String, Boolean>> methodCallback);
	
	@POST
	@Path("api/scenes/{sceneId}/dashboards")
	void addToZoneDashboard(@PathParam("sceneId") Long sceneId, @QueryParam("zoneId") Long zoneId, 
			MethodCallback<List<SceneDto>> methodCallback);
	
	@POST
	@Path("api/scenes/{sceneId}/dashboards")
	void addToProjectDashboard(@PathParam("sceneId") Long sceneId, @QueryParam("projectId") Long projectId, 
			MethodCallback<List<SceneDto>> methodCallback);
	
	@DELETE
	@Path("api/scenes/{sceneId}/dashboards")
	void removeFromZoneDashboard(@PathParam("sceneId") Long sceneId, @QueryParam("zoneId") Long zoneId, 
			MethodCallback<List<SceneDto>> methodCallback);
	
	@POST
	@Path("api/scenes/{sceneId}/dashboards")
	void removeFromProjectDashboard(@PathParam("sceneId") Long sceneId, @QueryParam("projectId") Long projectId, 
			MethodCallback<List<SceneDto>> methodCallback);
	
	@PUT
	@Path("api/scenedevices/{sceneDeviceId}")
	void saveSceneDevice(@PathParam("sceneDeviceId") Long sceneDeviceId, SceneDeviceDto sceneDeviceDto, 
			MethodCallback<SceneDeviceDto> methodCallback);
}
