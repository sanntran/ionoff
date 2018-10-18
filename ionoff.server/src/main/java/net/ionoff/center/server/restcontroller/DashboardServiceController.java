package net.ionoff.center.server.restcontroller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.persistence.service.IDashboardService;
import net.ionoff.center.shared.dto.DashboardDto;

@RestController
public class DashboardServiceController {

	@Autowired
	private IDashboardService dashboardService;

	
	@RequestMapping(value = "dashboards",
			params= {"projectId"},
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public DashboardDto findByProjectId(@RequestParam("projectId") Long projectId) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkProjectPermission(user, projectId);
		final DashboardDto dashboardDto = dashboardService.findDtoByUserProjectId(user, projectId);
		return dashboardDto;
	}


	@RequestMapping(value = "dashboards",
			params= {"zoneId"},
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")

	public DashboardDto findByZoneId(@RequestParam("zoneId") Long zoneId) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkZonePermission(user, zoneId);
		final DashboardDto dashboardDto = dashboardService.findDtoByUserZoneId(user, zoneId);
		return dashboardDto;
	}
}
