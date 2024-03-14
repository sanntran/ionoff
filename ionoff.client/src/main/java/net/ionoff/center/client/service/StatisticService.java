package net.ionoff.center.client.service;

import net.ionoff.center.shared.dto.AlertStatisticDto;
import org.fusesource.restygwt.client.MethodCallback;
import org.fusesource.restygwt.client.RestService;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.QueryParam;

public interface StatisticService extends RestService {
	
	@GET
	@Path("statistics/alert")
	void getAlertStatisticByZoneId(@QueryParam("zoneId") Long zoneId, MethodCallback<AlertStatisticDto> callback);
	
	@GET
	@Path("statistics/alert")
	void getAlertStatisticByProjectId(@QueryParam("projectId") Long projectId, MethodCallback<AlertStatisticDto> callback);

}
