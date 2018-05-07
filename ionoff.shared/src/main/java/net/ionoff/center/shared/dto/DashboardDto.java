package net.ionoff.center.shared.dto;

import java.util.List;

public class DashboardDto extends BaseDto {

	private static final long serialVersionUID = 1L;

	private int totalModeCount;
	private String activatedModeName;
	private int totalDeviceCount;
	private int deviceOnCount;
	private int deviceOffCount;
	private int totalRelayDriverCount;
	private int onlineRelayDriverCount;
	private int offlineRelayDriverCount;
	private int totalScheduleCount;
	private String nextSchedule;
	private String nextScheduleTime;
	private int totalSceneCount;
	private String lastPlayedSceneName;
	private int memoryUsedPercent;
	private int diskSpaceUsedPercent;
	
	private List<DeviceDto> devices;
	private List<SceneDto> scenes;

	public int getTotalModeCount() {
		return totalModeCount;
	}

	public void setTotalModeCount(int totalModeCount) {
		this.totalModeCount = totalModeCount;
	}

	public String getActivatedModeName() {
		return activatedModeName;
	}

	public void setActivatedModeName(String activatedModeName) {
		this.activatedModeName = activatedModeName;
	}

	public int getTotalDeviceCount() {
		return totalDeviceCount;
	}

	public void setTotalDeviceCount(int totalDeviceCount) {
		this.totalDeviceCount = totalDeviceCount;
	}

	public int getDeviceOnCount() {
		return deviceOnCount;
	}

	public void setDeviceOnCount(int deviceOnCount) {
		this.deviceOnCount = deviceOnCount;
	}

	public int getDeviceOffCount() {
		return deviceOffCount;
	}

	public void setDeviceOffCount(int deviceOffCount) {
		this.deviceOffCount = deviceOffCount;
	}

	public int getTotalScheduleCount() {
		return totalScheduleCount;
	}

	public void setTotalScheduleCount(int totalScheduleCount) {
		this.totalScheduleCount = totalScheduleCount;
	}

	public String getNextSchedule() {
		return nextSchedule;
	}

	public void setNextSchedule(String nextSchedule) {
		this.nextSchedule = nextSchedule;
	}

	public String getNextScheduleTime() {
		return nextScheduleTime;
	}

	public void setNextScheduleTime(String nextScheduleTime) {
		this.nextScheduleTime = nextScheduleTime;
	}

	public int getTotalSceneCount() {
		return totalSceneCount;
	}

	public void setTotalSceneCount(int totalSceneCount) {
		this.totalSceneCount = totalSceneCount;
	}

	public String getLastPlayedSceneName() {
		return lastPlayedSceneName;
	}

	public void setLastPlayedSceneName(String lastPlayedSceneName) {
		this.lastPlayedSceneName = lastPlayedSceneName;
	}


	public int getMemoryUsedPercent() {
		return memoryUsedPercent;
	}

	public void setMemoryUsedPercent(int memoryUsedPercent) {
		this.memoryUsedPercent = memoryUsedPercent;
	}

	public int getDiskSpaceUsedPercent() {
		return diskSpaceUsedPercent;
	}

	public void setDiskSpaceUsedPercent(int diskSpaceUsedPercent) {
		this.diskSpaceUsedPercent = diskSpaceUsedPercent;
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


	public int getTotalRelayDriverCount() {
		return totalRelayDriverCount;
	}

	public void setTotalRelayDriverCount(int totalRelayDriverCount) {
		this.totalRelayDriverCount = totalRelayDriverCount;
	}

	public int getOfflineRelayDriverCount() {
		return offlineRelayDriverCount;
	}

	public void setOfflineRelayDriverCount(int offlineRelayDriverCount) {
		this.offlineRelayDriverCount = offlineRelayDriverCount;
	}

	public int getOnlineRelayDriverCount() {
		return onlineRelayDriverCount;
	}

	public void setOnlineRelayDriverCount(int onlineRelayDriverCount) {
		this.onlineRelayDriverCount = onlineRelayDriverCount;
	}

}
