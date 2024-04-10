package net.ionoff.center.shared.dto;

import java.util.List;

public class DashboardDto extends BaseDto {

	private AlertStatisticDto alertStatistic;
	private AreaStatisticDto areaStatistic;
	private ZoneStatisticDto zoneStatistic;
	private SensorStatisticDto sensorStatistic;
	private ControllerStatisticDto controllerStatistic;
	private DeviceStatisticDto deviceStatistic;

	private ModeStatisticDto modeStatistic;
	private ScheduleStatisticDto scheduleStatistic;
	private SceneStatisticDto sceneStatistic;
	private ServerStatisticDto serverStatistic;
	private List<DeviceDto> devices;
	private List<SceneDto> scenes;

	public DeviceStatisticDto getDeviceStatistic() {
		return deviceStatistic;
	}

	public void setDeviceStatistic(DeviceStatisticDto deviceStatistic) {
		this.deviceStatistic = deviceStatistic;
	}

	public ModeStatisticDto getModeStatistic() {
		return modeStatistic;
	}

	public void setModeStatistic(ModeStatisticDto modeStatistic) {
		this.modeStatistic = modeStatistic;
	}

	public ScheduleStatisticDto getScheduleStatistic() {
		return scheduleStatistic;
	}

	public void setScheduleStatistic(ScheduleStatisticDto scheduleStatistic) {
		this.scheduleStatistic = scheduleStatistic;
	}

	public SceneStatisticDto getSceneStatistic() {
		return sceneStatistic;
	}

	public void setSceneStatistic(SceneStatisticDto sceneStatistic) {
		this.sceneStatistic = sceneStatistic;
	}

	public ServerStatisticDto getServerStatistic() {
		return serverStatistic;
	}

	public void setServerStatistic(ServerStatisticDto serverStatistic) {
		this.serverStatistic = serverStatistic;
	}

	public List<DeviceDto> getDevices() {
		return devices;
	}

	public void setDevices(List<DeviceDto> devices) {
		this.devices = devices;
	}

	public List<SceneDto> getScenes() {
		return scenes;
	}

	public void setScenes(List<SceneDto> scenes) {
		this.scenes = scenes;
	}

	public ControllerStatisticDto getControllerStatistic() {
		return controllerStatistic;
	}

	public void setControllerStatistic(ControllerStatisticDto controllerStatistic) {
		this.controllerStatistic = controllerStatistic;
	}

	public AlertStatisticDto getAlertStatistic() {
		return alertStatistic;
	}

	public void setAlertStatistic(AlertStatisticDto alertStatistic) {
		this.alertStatistic = alertStatistic;
	}

	public AreaStatisticDto getAreaStatistic() {
		return areaStatistic;
	}

	public void setAreaStatistic(AreaStatisticDto areaStatistic) {
		this.areaStatistic = areaStatistic;
	}

	public ZoneStatisticDto getZoneStatistic() {
		return zoneStatistic;
	}

	public void setZoneStatistic(ZoneStatisticDto zoneStatistic) {
		this.zoneStatistic = zoneStatistic;
	}

	public SensorStatisticDto getSensorStatistic() {
		return sensorStatistic;
	}

	public void setSensorStatistic(SensorStatisticDto sensorStatistic) {
		this.sensorStatistic = sensorStatistic;
	}
}
