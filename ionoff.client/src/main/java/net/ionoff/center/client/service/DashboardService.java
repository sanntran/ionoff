package net.ionoff.center.client.service;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.QueryParam;

import org.fusesource.restygwt.client.MethodCallback;

import net.ionoff.center.shared.dto.DashboardDto;

/**
 * @author Sann Tran
 */
public interface DashboardService extends EntityService<DashboardDto> {
	
	@GET
	@Path("dashboards/{dashboardId}")
	void findById(@PathParam("dashboardId") Long dashboardId, MethodCallback<DashboardDto> callback);
	
	@GET
	@Path("dashboards")
	void findByZoneId(@QueryParam("zoneId") Long zoneId, MethodCallback<DashboardDto> callback);
	
	@GET
	@Path("dashboards")
	void findByProjectId(@QueryParam("projectId") Long projectId, MethodCallback<DashboardDto> callback);

}
