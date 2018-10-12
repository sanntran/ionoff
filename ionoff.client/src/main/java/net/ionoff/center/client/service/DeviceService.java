package net.ionoff.center.client.service;

import java.util.List;

import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.QueryParam;

import org.fusesource.restygwt.client.MethodCallback;

import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.QueryCriteriaDto;
import net.ionoff.center.shared.dto.StatusDto;

/**
 * @author Sann Tran
 */
public interface DeviceService extends EntityService<DeviceDto> {

	@PUT
	@Path("devices/{deviceId}")
	void save(@PathParam("deviceId") Long deviceId, DeviceDto deviceDto, MethodCallback<DeviceDto> callback);
	
	@Override
	@DELETE
	@Path("devices/{deviceId}")
	void delete(@PathParam("deviceId") Long deviceId, MethodCallback<MessageDto> callback);
	
	@Override
	@POST
	@Path("devices/search")
	void searchByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<List<DeviceDto>> callback);
	
	@Override 
	@POST
	@Path("devices/count")
	void countByCriteria(QueryCriteriaDto criteriaDto,
			MethodCallback<Long> callback);
	
	@GET
	@Path("devices/{deviceId}")
	void findById(@PathParam("deviceId") Long deviceId, MethodCallback<DeviceDto> callback);
	
	@GET
	@Path("devices")
	void findByProjectId(@QueryParam("projectId") Long projectId, MethodCallback<List<DeviceDto>> callback);
	
	@GET
	@Path("devices")
	void findByZoneId(@QueryParam("zoneId") Long zoneId, MethodCallback<List<DeviceDto>> callback);
	
	@POST
	@Path("devices/{deviceId}/off")
	void turnOffDevice(@PathParam("deviceId") Long deviceId, MethodCallback<StatusDto> callback);
	
	@POST
	@Path("devices/{deviceId}/on")
	void turnOnDevice(@PathParam("deviceId") Long deviceId, MethodCallback<StatusDto> callback);
	
	@GET
	@Path("devices/status")
	void getStatusByZoneId(@QueryParam("zoneId") Long zoneId, MethodCallback<List<StatusDto>> methodCallback);
	
	@GET
	@Path("devices/status")
	void getStatusByProjectId(@QueryParam("projectId") Long projectId, MethodCallback<List<StatusDto>> methodCallback);
	
	@POST
	@Path("devices/{deviceId}/dashboards")
	void addToZoneDashboard(@PathParam("deviceId") Long deviceId, @QueryParam("zoneId") Long zoneId, 
			MethodCallback<DeviceDto> methodCallback);
	
	@POST
	@Path("devices/{deviceId}/dashboards")
	void addToProjectDashboard(@PathParam("deviceId") Long deviceId, @QueryParam("projectId") Long projectId, 
			MethodCallback<MessageDto> methodCallback);
	
	@DELETE
	@Path("devices/{deviceId}/dashboards")
	void removeFromZoneDashboard(@PathParam("deviceId") Long deviceId, @QueryParam("zoneId") Long zoneId, 
			MethodCallback<MessageDto> methodCallback);
	
	@DELETE
	@Path("devices/{deviceId}/dashboards")
	void removeFromProjectDashboard(@PathParam("deviceId") Long deviceId, @QueryParam("projectId") Long projectId, 
			MethodCallback<MessageDto> methodCallback);
}
