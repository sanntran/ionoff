package net.ionoff.center.shared.dto;

import java.util.List;

public class DashboardDto extends BaseDto {

	private List<ZoneDto> zones;
	private List<DeviceDto> devices;
	private List<SceneDto> scenes;

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

	public List<ZoneDto> getZones() {
		return zones;
	}

	public void setZones(List<ZoneDto> zones) {
		this.zones = zones;
	}
}
