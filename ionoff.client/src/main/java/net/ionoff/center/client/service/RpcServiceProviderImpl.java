package net.ionoff.center.client.service;

import com.google.gwt.event.shared.HandlerManager;
import com.google.inject.Inject;

import net.ionoff.center.client.event.ShowLoadingEvent;
import net.xapxinh.center.client.player.rpc.PlayerService;

public class RpcServiceProviderImpl implements IRpcServiceProvider {

	@Inject
	protected HandlerManager eventBus;
	
	@Inject
	private LoginService loginAsyn;
	@Inject
	protected PlayerService playerService;
	@Inject
	private AreaService areaService;
	@Inject
	private ControllerService controllerService;
	@Inject
	private DeviceService deviceService;
	@Inject
	private ProjectService projectService;
	@Inject
	private ModeSceneService modeSceneService;
	@Inject
	private ModeSensorSceneService modeSensorSceneService;
	@Inject
	private ModeSensorService modeSensorService;
	@Inject
	private ModeSensorUserService modeSensorUserService;
	@Inject
	private ModeService modeService;
	@Inject
	private RelayService relayService;
	@Inject
	private SceneActionService sceneActionService;
	@Inject
	private ScenePlayerActionService scenePlayerActionService;
	@Inject
	private SceneRelayActionService sceneRelayActionService;
	@Inject
	private SceneService sceneService;
	@Inject
	private SchedulePlayerActionService schedulePlayerActionService;
	@Inject
	private ScheduleRelayActionService scheduleRelayActionService;
	@Inject
	private ScheduleService scheduleService;
	@Inject
	private SensorService sensorService;
	@Inject
	private ZoneService zoneService;
	@Inject
	private UserService userService;
	@Inject
	private SystemService systemService;
	@Inject
	private DashboardService dashboardService;
	@Inject
	private SensorDataService sensorDataService;
	
	@Override
	public LoginService getLoginService() {
		return loginAsyn;
	}

	@Override
	public AreaService getAreaService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return areaService;
	}

	@Override
	public ControllerService getControllerService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return controllerService;
	}

	@Override
	public DeviceService getDeviceService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return deviceService;
	}

	@Override
	public ProjectService getProjectService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return projectService;
	}

	@Override
	public ModeSceneService getModeSceneService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return modeSceneService;
	}

	@Override
	public ModeSensorSceneService getModeSensorSceneService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return modeSensorSceneService;
	}

	@Override
	public ModeSensorService getModeSensorService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return modeSensorService;
	}

	@Override
	public ModeSensorUserService getModeSensorUserService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return modeSensorUserService;
	}

	@Override
	public ModeService getModeService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return modeService;
	}

	@Override
	public RelayService getRelayService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return relayService;
	}

	@Override
	public SceneActionService getSceneActionService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return sceneActionService;
	}

	@Override
	public ScenePlayerActionService getScenePlayerActionService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return scenePlayerActionService;
	}

	@Override
	public SceneRelayActionService getSceneRelayActionService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return sceneRelayActionService;
	}

	@Override
	public SceneService getSceneService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return sceneService;
	}

	@Override
	public SchedulePlayerActionService getSchedulePlayerActionService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return schedulePlayerActionService;
	}

	@Override
	public ScheduleRelayActionService getScheduleRelayActionService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return scheduleRelayActionService;
	}

	@Override
	public ScheduleService getScheduleService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return scheduleService;
	}

	@Override
	public SensorService getSensorService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return sensorService;
	}

	@Override
	public ZoneService getZoneService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return zoneService;
	}

	@Override
	public UserService getUserService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return userService;
	}
	
	@Override
	public PlayerService getPlayerService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return playerService;
	}
	
	@Override
	public SystemService getSystemService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return systemService;
	}

	@Override
	public DashboardService getDashboardService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return dashboardService;
	}

	@Override
	public SensorDataService getSensorDataService() {
		eventBus.fireEvent(ShowLoadingEvent.getInstance(true));
		return sensorDataService;
	}
}
