package net.ionoff.center.server.persistence.service.impl;

import net.ionoff.center.server.entity.*;
import net.ionoff.center.server.exception.EntityNotFoundException;
import net.ionoff.center.server.mediaplayer.service.IMediaPlayerService;
import net.ionoff.center.server.persistence.dao.*;
import net.ionoff.center.server.persistence.mapper.*;
import net.ionoff.center.server.persistence.service.IDashboardService;
import net.ionoff.center.server.persistence.service.IDeviceService;
import net.ionoff.center.server.persistence.service.IProjectService;
import net.ionoff.center.server.persistence.service.IZoneService;
import net.ionoff.center.server.util.DateTimeUtil;
import net.ionoff.center.shared.dto.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

@Service
@Transactional
public class DashboardServiceImpl extends AbstractGenericService<Dashboard, DashboardDto> implements IDashboardService {
	
	private IDashboardDao dashboardDao;
	
	@Autowired
	private DashboardMapper dashboardMapper;

	@Autowired
	private DeviceMapper deviceMapper;

	@Autowired
	private SensorMapper sensorMapper;

	@Autowired
	private ZoneMapper zoneMapper;

	@Autowired
	private IZoneDao zoneDao;

	@Autowired
	private IAreaDao areaDao;

	@Autowired
	private AreaMapper areaMapper;

	@Autowired
	private ControllerMapper controllerMapper;

	@Autowired
	private IDeviceDao deviceDao;
	
	@Autowired
	private ISceneDao sceneDao;
	
	@Autowired
	private IDashboardDeviceDao dashboardDeviceDao;
	
	@Autowired
	private IDashboardSceneDao dashboardSceneDao;
	
	@Autowired
	private IMediaPlayerService playerService;

	@Autowired
	private IProjectService projectService;

	@Autowired
	private IScheduleDao scheduleDao;

	@Autowired
	private IModeDao modeDao;

	@Autowired
	private IControllerDao controllerDao;

    @Autowired
    private IZoneService zoneService;

    @Lazy
    @Autowired
    private IDeviceService deviceService;

	@Autowired
	private ISensorDao sensorDao;

	@Autowired
	public DashboardServiceImpl(IDashboardDao dashboardDao) {
		this.dashboardDao = dashboardDao;
	}
	
	@Override
	protected IDashboardDao getDao() {
		return dashboardDao;
	}

	@Override
	public DashboardDto requireDtoById(long id) {
		Dashboard dashboard = dashboardDao.findById(id);
		return dashboardMapper.createDto(dashboard);
	}

	@Override
	public DashboardDto insertDto(User user, DashboardDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public DashboardDto updateDto(User user, DashboardDto dto) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void deleteDtoById(User user, long id) {
		throw new UnsupportedOperationException();
	}

	@Override
	public Dashboard findByUserZoneId(User user, long zoneId) {
		return dashboardDao.findByUserZoneId(user.getId(), zoneId);
	}

	@Override
	public Dashboard findByUserProjectId(User user, long projectId) {
		return dashboardDao.findByUserProjectId(user.getId(), projectId);
	}

	@Override
	protected List<DashboardDto> createDtoList(List<Dashboard> entities) {
		List<DashboardDto> dashboardDtos = new ArrayList<>();
		for (Dashboard dashboard : entities) {
			dashboardDtos.add(dashboardMapper.createDto(dashboard));
		}
		return dashboardDtos;
	}

	@Override
	public void removeByUserProject(User user, long projectId) {
		dashboardDao.removeByUserProjectId(user.getId(), projectId);
	}

	@Override
	public DashboardDto findDtoByUserZoneId(User user, long zoneId) {
		Dashboard dashboard = findByUserZoneId(user, zoneId);
		DashboardDto dashboardDto = dashboardMapper.createDto(dashboard);

		List<Device> devices = deviceDao.findByUserZoneId(user.getId(), zoneId);
		List<Schedule> schedules = scheduleDao.findByZoneId(zoneId);

		setDeviceStatistic(devices, dashboardDto);
		setScheduleStatistic(user, schedules, dashboardDto);

		SceneStatisticDto sceneStatistic = new SceneStatisticDto();
		sceneStatistic.setTotalCount((int)sceneDao.countByZoneId(zoneId));
		dashboardDto.setSceneStatistic(sceneStatistic);

		List<DeviceDto> deviceDtos = deviceService.findDtoByUserZoneId(user, zoneId);
		dashboardDto.setDevices(deviceDtos);
		return dashboardDto;
	}

	@Override
	public DashboardDto findDtoByUserProjectId(User user, long projectId) {
		Dashboard dashboard = findByUserProjectId(user, projectId);
		DashboardDto dashboardDto = dashboardMapper.createDto(dashboard);

		List<Device> devices = deviceDao.findByStatus(user.getId(), projectId, true);
		List<Schedule> schedules = scheduleDao.findByProjectId(projectId);

		setDeviceStatistic(devices, dashboardDto);

		setServerStatistic(dashboardDto);
		setModeStatistic(user, projectId, dashboardDto);
		setSceneStatistic(user, projectId, dashboardDto);
		setScheduleStatistic(user, schedules, dashboardDto);
		setControllerStatistic(projectId, dashboardDto);
		setSensorStatistic(projectId, dashboardDto);
		setAreaStatistic(projectId, dashboardDto);
		setZoneStatistic(projectId, dashboardDto);
		setAlertStatistic(projectId, dashboardDto);
		return dashboardDto;
	}

	private void setAreaStatistic(long projectId, DashboardDto dashboardDto) {
		List<Area> alertArea = areaDao.findHavingAlertInProject(projectId);
		AreaStatisticDto areaStatistic = new AreaStatisticDto();
		areaStatistic.setHavingAlertCount(alertArea.size());
		areaStatistic.setFirstHasAlert(alertArea.isEmpty() ? null : areaMapper.createDto(alertArea.get(0)));
		dashboardDto.setAreaStatistic(areaStatistic);
	}

	private void setZoneStatistic(long projectId, DashboardDto dashboardDto) {
		List<Zone> alertZone = zoneDao.findHavingAlertInProject(projectId);
		ZoneStatisticDto zoneStatistic = new ZoneStatisticDto();
		zoneStatistic.setHavingAlertCount(alertZone.size());
		zoneStatistic.setFirstHasAlert(alertZone.isEmpty() ? null : zoneMapper.createZoneDto(alertZone.get(0), false));
		dashboardDto.setZoneStatistic(zoneStatistic);
	}

	private void setAlertStatistic(long projectId, DashboardDto dashboardDto) {
		List<Sensor> alertSensors = sensorDao.findHavingAlertInProject(projectId);
		AlertStatisticDto alertStatistic = new AlertStatisticDto();
		alertStatistic.setTotalCount(alertSensors.size());
		alertStatistic.setFirstAlert(alertSensors.isEmpty() ? null : sensorMapper.createSensorDto(alertSensors.get(0)));
		dashboardDto.setAlertStatistic(alertStatistic);
	}

	private void setSensorStatistic(long projectId, DashboardDto dashboardDto) {
		List<Sensor> offlineSensors = sensorDao.findOfflineInProject(projectId);
		SensorStatisticDto sensorStatistic = new SensorStatisticDto();
		sensorStatistic.setOfflineCount(offlineSensors.size());
		sensorStatistic.setFirstOffline(offlineSensors.isEmpty() ? null : sensorMapper.createSensorDto(offlineSensors.get(0)));
		dashboardDto.setSensorStatistic(sensorStatistic);
	}

	private void setServerStatistic(DashboardDto dashboardDto) {
		ServerStatisticDto serverStatistic = new ServerStatisticDto();
		long totalMem = Runtime.getRuntime().totalMemory();
		long freeMem = Runtime.getRuntime().freeMemory();
		int usedMemPercent = (int) (100 - freeMem * 100 /totalMem);
		serverStatistic.setMemoryUsedPercent(usedMemPercent);

		/* Get a list of all filesystem roots on this system */
		File[] roots = File.listRoots();

		serverStatistic.setDiskSpaceUsedPercent(0);
		if (roots.length > 0) {
			long totalSpace = roots[0].getTotalSpace();
			long freeSpace = roots[0].getFreeSpace();
			int usedSpacePercent = (int) (100 - freeSpace * 100 /totalSpace);

			serverStatistic.setDiskSpaceUsedPercent(usedSpacePercent);
		}

		dashboardDto.setServerStatistic(serverStatistic);
	}

	private void setControllerStatistic(long projectId, DashboardDto dashboardDto) {
		ControllerStatisticDto controllerStatistic = new ControllerStatisticDto();
		List<Controller> controllers = controllerDao.findOfflineInProject(projectId);
		controllerStatistic.setOfflineCount(controllers.size());
		controllerStatistic.setFirstOffline(controllers.isEmpty() ? null : controllerMapper.createControllerDto(controllers.get(0)));
		dashboardDto.setControllerStatistic(controllerStatistic);
	}

	private void setDeviceStatistic(List<Device> devices, DashboardDto dashboardDto) {
		DeviceStatisticDto deviceStatistic = new DeviceStatisticDto();
		deviceStatistic.setTotalOn(devices.size());
		Device firstOn = devices.stream().findFirst().orElse(null);
		deviceStatistic.setDeviceOn(firstOn == null ? null : deviceMapper.createDeviceDto(firstOn));
		dashboardDto.setDeviceStatistic(deviceStatistic);
	}

	private void setModeStatistic(User user, long projectId, DashboardDto dashboardDto) {
		ModeStatisticDto modeStatistic  = new ModeStatisticDto();
		modeStatistic.setTotalCount((int)modeDao.countByProjectId(projectId));
		Mode activatedMode = modeDao.findByLastActivated(projectId);
		if (activatedMode == null) {
			modeStatistic.setActivatedName("");
		}
		else {
			modeStatistic.setActivatedName(activatedMode.getName());
		}
		dashboardDto.setModeStatistic(modeStatistic);
	}

	private void setSceneStatistic(User user, long projectId, DashboardDto dashboardDto) {
		SceneStatisticDto sceneStatistic = new SceneStatisticDto();
		sceneStatistic.setTotalCount((int)sceneDao.countByProjectId(projectId));
		dashboardDto.setSceneStatistic(sceneStatistic);
	}

	private void setScheduleStatistic(User user, List<Schedule> schedules, DashboardDto dashboardDto) {
		ScheduleStatisticDto scheduleStatistic = new ScheduleStatisticDto();
		scheduleStatistic.setTotalCount(schedules.size());
		Schedule nextScheduleToday = getNextScheduleToday(schedules);
		if (nextScheduleToday != null) {
			scheduleStatistic.setNextScheduleName(nextScheduleToday.getName());
			scheduleStatistic.setNextScheduleTime(revertFormmatedTime(nextScheduleToday.getTime()));
		}
		dashboardDto.setScheduleStatistic(scheduleStatistic);
	}

	@Override
	public void addDeviceToZoneDashboard(User user, Long deviceId) {
		Device device = requireDeviceById(deviceId);
		Dashboard dashboard = dashboardDao.findByUserZoneId(user.getId(), device.getZone().getId());
		for (DashboardDevice dashboardDevice : dashboard.getDevices()) {
			if (dashboardDevice.getDevice().getId() == deviceId) {
				return;
			}
		}
		DashboardDevice dashboardDevice = new DashboardDevice();
		dashboardDevice.setDashboard(dashboard);
		dashboardDevice.setDevice(device);
		dashboardDevice.setProject(device.getProject());
		dashboardDeviceDao.insert(dashboardDevice);
	}
	
	@Override
	public void removeDeviceFromZoneDashboard(User user, Long deviceId) {
		Device device = requireDeviceById(deviceId);
		Dashboard dashboard = dashboardDao.findByUserZoneId(user.getId(), device.getZone().getId());
		DashboardDevice dashboardDevice = dashboardDeviceDao.findByDashboardDeviceId(dashboard.getId(), device.getId());
		if (dashboardDevice != null) {
			dashboardDeviceDao.delete(dashboardDevice);
		}
	}

	@Override
	public void addDeviceToProjectDashboard(User user, Long deviceId) {
		Device device = requireDeviceById(deviceId);
		Dashboard dashboard = dashboardDao.findByUserProjectId(user.getId(), device.getProject().getId());
		for (DashboardDevice dashboardDevice : dashboard.getDevices()) {
			if (dashboardDevice.getDevice().getId() == deviceId) {
				return;
			}
		}
		DashboardDevice dashboardDevice = new DashboardDevice();
		dashboardDevice.setDashboard(dashboard);
		dashboardDevice.setDevice(device);
		dashboardDevice.setProject(device.getProject());
		dashboardDeviceDao.insert(dashboardDevice);
	}
	
	@Override
	public void removeDeviceFromProjectDashboard(User user, Long deviceId) {
		Device device = requireDeviceById(deviceId);
		Dashboard dashboard = dashboardDao.findByUserProjectId(user.getId(), device.getProject().getId());
		DashboardDevice dashboardDevice = dashboardDeviceDao.findByDashboardDeviceId(dashboard.getId(), device.getId());
		if (dashboardDevice != null) {
			dashboardDeviceDao.delete(dashboardDevice);
		}
	}
	
	@Override
	public void addSceneToZoneDashboard(User user, Long sceneId) {
		Scene scene = requireSceneById(sceneId);
		Dashboard dashboard = dashboardDao.findByUserZoneId(user.getId(), scene.getZone().getId());
		DashboardScene dashboardScene = new DashboardScene();
		dashboardScene.setDashboard(dashboard);
		dashboardScene.setScene(scene);
		dashboardScene.setProject(scene.getZone().getProject());
		dashboardSceneDao.insert(dashboardScene);
	}
	
	@Override
	public void removeSceneFromZoneDashboard(User user, Long sceneId) {
		Scene scene = requireSceneById(sceneId);
		Dashboard dashboard = dashboardDao.findByUserZoneId(user.getId(), scene.getZone().getId());
		DashboardScene dashboardScene = dashboardSceneDao.findByDashboardSceneId(dashboard.getId(), scene.getId());
		if (dashboardScene != null) {
			dashboardSceneDao.delete(dashboardScene);
		}
	}

	@Override
	public void addSceneToProjectDashboard(User user, Long sceneId) {
		Scene scene = requireSceneById(sceneId);
		Dashboard dashboard = dashboardDao.findByUserProjectId(user.getId(), scene.getZone().getProject().getId());
		DashboardScene dashboardScene = new DashboardScene();
		dashboardScene.setDashboard(dashboard);
		dashboardScene.setScene(scene);
		dashboardScene.setProject(scene.getZone().getProject());
		dashboardSceneDao.insert(dashboardScene);
	}
	
	@Override
	public void removeSceneFromProjectDashboard(User user, Long sceneId) {
		Scene scene = requireSceneById(sceneId);
		Dashboard dashboard = dashboardDao.findByUserProjectId(user.getId(), scene.getZone().getProject().getId());
		DashboardScene dashboardScene = dashboardSceneDao.findByDashboardSceneId(dashboard.getId(), scene.getId());
		if (dashboardScene != null) {
			dashboardSceneDao.delete(dashboardScene);
		}
	}
	
	private Device requireDeviceById(Long deviceId) {
		Device d = deviceDao.findById(deviceId);
		if (d == null) {
			throw new EntityNotFoundException(deviceId, Device.class.getSimpleName());
		}
		return d;
	}

	private Scene requireSceneById(Long sceneId) {
		Scene s = sceneDao.findById(sceneId);
		if (s == null) {
			throw new EntityNotFoundException(sceneId, Device.class.getSimpleName());
		}
		return s;
	}


	private String revertFormmatedTime(String time) {
		if (time.contains(DateTimeUtil.AM)) {
			return  time.replaceAll(DateTimeUtil.AM + " ", "") + " " + DateTimeUtil.AM;
		}
		else {
			return time.replaceAll(DateTimeUtil.PM + " ", "") + " " + DateTimeUtil.PM;
		}
	}

	private Schedule getNextScheduleToday(List<Schedule> schedules) {
		Date today = new Date();
		String todayYmd = new SimpleDateFormat(ScheduleConst.DATE_FORMAT).format(today);
		String dayOfWeek = new SimpleDateFormat("EEE").format(today);
		List<Schedule> todayEnabledSchedules = new ArrayList<>();
		for (Schedule schedule : schedules) {
			if (schedule.getEnabled()) {
				if (ScheduleConst.REPEAT_ONCE.equals(schedule.getRepeat())) {
					if (todayYmd.equals(schedule.getDay())) {
						schedule.setTime(formatTimeToCompare(schedule.getTime()));
						todayEnabledSchedules.add(schedule);
					}
				}
				else if (schedule.getDay().contains(dayOfWeek)) {
					schedule.setTime(formatTimeToCompare(schedule.getTime()));
					todayEnabledSchedules.add(schedule);
				}
			}
		}
		if (todayEnabledSchedules.isEmpty()) {
			return null;
		}
		else if (todayEnabledSchedules.size() == 1) {
			return todayEnabledSchedules.get(0);
		}
		Collections.sort(todayEnabledSchedules, Comparator.comparing(Schedule::getTime));
		String timeNow = new SimpleDateFormat("a hh:mm").format(today);
		for (Schedule schedule : schedules) {
			if (schedule.getTime().compareTo(timeNow) > 0) {
				return schedule;
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

}
