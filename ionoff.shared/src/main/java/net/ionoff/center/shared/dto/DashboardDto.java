package net.ionoff.center.shared.dto;

import java.util.List;

public class DashboardDto extends BaseDto {

	private DeviceStatisticDto deviceStatistic;
	private ModeStatisticDto modeStatistic;
	private ScheduleStatisticDto scheduleStatistic;
	private SceneStatisticDto sceneStatistic;
	private ServerStatisticDto serverStatistic;
	private RelayDriverStatisticDto relayDriverStatisticDto;
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

	public RelayDriverStatisticDto getRelayDriverStatisticDto() {
		return relayDriverStatisticDto;
	}

	public void setRelayDriverStatisticDto(RelayDriverStatisticDto relayDriverStatisticDto) {
		this.relayDriverStatisticDto = relayDriverStatisticDto;
	}
}
