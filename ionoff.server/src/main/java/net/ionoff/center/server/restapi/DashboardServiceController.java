package net.ionoff.center.server.restapi;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;

import net.ionoff.center.server.persistence.service.IModeService;
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
@EnableWebMvc
public class DashboardServiceController {

	@Autowired
	private IDashboardService dashboardService;
	@Autowired
	private IProjectService projectService;
	@Autowired
	private ISceneService sceneService;
	@Autowired
	private IDeviceService deviceService;
	@Autowired
	private IScheduleService scheduleService;
	
	@RequestMapping(value = "dashboards",
			params= {"projectId"},
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public DashboardDto findByProjectId(@RequestParam("projectId") Long projectId) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkProjectPermission(user, projectId);
		final DashboardDto dashboardDto = dashboardService.findDtoByUserProjectId(user, projectId);

		final Project project = projectService.findById(projectId);

		final List<DeviceDto> devices = deviceService.findDtoByUserProjectId(user, projectId);
		final List<ScheduleDto> scheduleDtos = scheduleService.findDtoByUserProject(user, projectId);
		final List<SceneDto> scenceDtos = sceneService.findDtoByUserProject(user, projectId);
		
		dashboardDto.setTotalModeCount(project.getModes().size());

		Mode activatedMode = project.getActivatedMode();
		if (activatedMode == null) {
			dashboardDto.setActivatedModeName("");
		}
		else {
			dashboardDto.setActivatedModeName(activatedMode.getName());
		}

		dashboardDto.setTotalDeviceCount(devices.size());
		for (DeviceDto device : devices) {
			StatusDto status = device.getStatus();
			if (status.getValue() != null) {
				if (status.getValue()) {
					dashboardDto.setDeviceOnCount(dashboardDto.getDeviceOnCount() + 1);
				}
				else {
					dashboardDto.setDeviceOffCount(dashboardDto.getDeviceOffCount() + 1);
				}
			}
		}
		
		ScheduleDto nextScheduleToday = getNextScheduleToday(scheduleDtos); 
		if (nextScheduleToday != null) {
			nextScheduleToday.setTime(revertFormmatedTime(nextScheduleToday.getTime()));
			dashboardDto.setNextSchedule(nextScheduleToday.getName());
			dashboardDto.setNextScheduleTime(nextScheduleToday.getTime());
		}
		dashboardDto.setTotalScheduleCount(scheduleDtos.size());
		dashboardDto.setTotalSceneCount(scenceDtos.size());
		
		dashboardDto.setTotalRelayDriverCount(project.getRelayDrivers().size());
		for (RelayDriver relayDriver : project.getRelayDrivers()) {
			if (relayDriver.isConnected()) {
				dashboardDto.setOnlineRelayDriverCount(dashboardDto.getOnlineRelayDriverCount() + 1);
			}
			else {
				dashboardDto.setOfflineRelayDriverCount(dashboardDto.getOfflineRelayDriverCount() + 1);
			}
		}
		
		long totalMem = Runtime.getRuntime().totalMemory();
		long freeMem = Runtime.getRuntime().freeMemory();
		
		int usedMemPercent = (int) (100 - freeMem * 100 /totalMem);
		
		dashboardDto.setMemoryUsedPercent(usedMemPercent);
		
		/* Get a list of all filesystem roots on this system */
		File[] roots = File.listRoots();

		dashboardDto.setDiskSpaceUsedPercent(0);
		if (roots.length > 0) {
			long totalSpace = roots[0].getTotalSpace();
			long freeSpace = roots[0].getFreeSpace();
			int usedSpacePercent = (int) (100 - freeSpace * 100 /totalSpace);
			
			dashboardDto.setDiskSpaceUsedPercent(usedSpacePercent);
		}		
		
		return dashboardDto;
	}

	private String revertFormmatedTime(String time) {
		if (time.contains(DateTimeUtil.AM)) {
			return  time.replaceAll(DateTimeUtil.AM + " ", "") + " " + DateTimeUtil.AM;
		}
		else {
			return time.replaceAll(DateTimeUtil.PM + " ", "") + " " + DateTimeUtil.PM;
		}
	}

	private ScheduleDto getNextScheduleToday(List<ScheduleDto> scheduleDtos) {
		Date today = new Date();
		String todayYmd = new SimpleDateFormat(ScheduleConst.DATE_FORMAT).format(today);
		String dayOfWeek = new SimpleDateFormat("EEE").format(today);
		List<ScheduleDto> todayEnabledSchedules = new ArrayList<>();
		for (ScheduleDto scheduleDto : scheduleDtos) {
			if (scheduleDto.getEnabled()) {
				if (ScheduleConst.REPEAT_ONCE.equals(scheduleDto.getRepeat())) {
					if (todayYmd.equals(scheduleDto.getDay())) {
						scheduleDto.setTime(formatTimeToCompare(scheduleDto.getTime()));
						todayEnabledSchedules.add(scheduleDto);
					}
				}
				else if (scheduleDto.getDay().contains(dayOfWeek)) {
					scheduleDto.setTime(formatTimeToCompare(scheduleDto.getTime()));
					todayEnabledSchedules.add(scheduleDto);
				}
			}
		}
		if (todayEnabledSchedules.isEmpty()) {
			return null;
		}
		else if (todayEnabledSchedules.size() == 1) {
			return todayEnabledSchedules.get(0);
		}
		Collections.sort(todayEnabledSchedules, new Comparator<ScheduleDto>() {
			@Override
			public int compare(ScheduleDto s1, ScheduleDto s2) {
				return s1.getTime().compareTo(s2.getTime());
			}
		});
		String timeNow = new SimpleDateFormat("a hh:mm").format(today);
		for (ScheduleDto scheduleDto : scheduleDtos) {
			if (scheduleDto.getTime().compareTo(timeNow) > 0) {
				return scheduleDto;
			}
		}
		return null;
	}
	
	private String formatTimeToCompare(String time) {
		if (time.contains(DateTimeUtil.AM)) {
			return DateTimeUtil.AM + " " + time.replaceAll(" " + DateTimeUtil.AM, "");
		}
		else {
			return DateTimeUtil.PM + " " + time.replaceAll(" " + DateTimeUtil.PM, "");
		}
	}

	@RequestMapping(value = "dashboards",
			params= {"zoneId"},
			method = RequestMethod.GET,
			produces = "application/json; charset=utf-8")
	@ResponseBody
	public DashboardDto findByZoneId(@RequestParam("zoneId") Long zoneId) {
		User user = RequestContextHolder.getUser();
		RequestContextHolder.checkZonePermission(user, zoneId);
		final DashboardDto dashboardDto = dashboardService.findDtoByUserZoneId(user, zoneId);
		
		final List<DeviceDto> devices = deviceService.findDtoByUserZoneId(user, zoneId);
		final List<ScheduleDto> scheduleDtos = scheduleService.findDtoByUserZone(user, zoneId);
		final List<SceneDto> scenceDtos = sceneService.findDtoByUserZone(user, zoneId);
		
		dashboardDto.setTotalModeCount(0);
		dashboardDto.setActivatedModeName("");
		
		dashboardDto.setTotalDeviceCount(devices.size());
		for (DeviceDto device : devices) {
			StatusDto status = device.getStatus();
			if (status.getValue() != null) {
				if (status.getValue()) {
					dashboardDto.setDeviceOnCount(dashboardDto.getDeviceOnCount() + 1);
				}
				else {
					dashboardDto.setDeviceOffCount(dashboardDto.getDeviceOffCount() + 1);
				}
			}
		}
		ScheduleDto nextScheduleToday = getNextScheduleToday(scheduleDtos); 
		if (nextScheduleToday != null) {
			nextScheduleToday.setTime(revertFormmatedTime(nextScheduleToday.getTime()));
			dashboardDto.setNextSchedule(nextScheduleToday.getName());
			dashboardDto.setNextScheduleTime(nextScheduleToday.getTime());
		}
		dashboardDto.setTotalScheduleCount(scheduleDtos.size());
		dashboardDto.setTotalSceneCount(scenceDtos.size());
		
		return dashboardDto;
	}
}
