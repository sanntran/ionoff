package net.ionoff.center.server.controller;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;

import net.ionoff.center.server.entity.Mode;
import net.ionoff.center.server.entity.Project;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.entity.User;
import net.ionoff.center.server.persistence.service.IDashboardService;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.persistence.service.IProjectService;
import net.ionoff.center.server.persistence.service.ISceneService;
import net.ionoff.center.server.persistence.service.IScheduleService;
import net.ionoff.center.server.util.DateTimeUtil;
import net.ionoff.center.shared.dto.DashboardDto;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.SceneDto;
import net.ionoff.center.shared.dto.ScheduleConst;
import net.ionoff.center.shared.dto.ScheduleDto;
import net.ionoff.center.shared.dto.StatusDto;

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
