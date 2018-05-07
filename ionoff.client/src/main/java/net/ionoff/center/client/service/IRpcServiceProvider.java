package net.ionoff.center.client.service;

import net.xapxinh.center.client.player.rpc.PlayerService;

public interface IRpcServiceProvider {
	
	SystemService getSystemService();
	
	LoginService getLoginService();
	
	PlayerService getPlayerService();

	AreaService getAreaService();

	RelayDriverService getRelayDriverService();

	DeviceService getDeviceService();

	ProjectService getProjectService();

	ModeSceneService getModeSceneService();

	ModeSensorSceneService getModeSensorSceneService();

	ModeSensorService getModeSensorService();

	ModeSensorUserService getModeSensorUserService();

	ModeService getModeService();

	RelayService getRelayService();

	SceneActionService getSceneActionService();

	ScenePlayerActionService getScenePlayerActionService();

	SceneRelayActionService getSceneRelayActionService();

	SceneService getSceneService();

	SchedulePlayerActionService getSchedulePlayerActionService();

	ScheduleRelayActionService getScheduleRelayActionService();

	ScheduleService getScheduleService();

	SensorService getSensorService();

	ZoneService getZoneService();

	UserService getUserService();

	DashboardService getDashboardService();

	SensorDataService getSensorDataService();
}
