package net.ionoff.center.shared.dto;

import java.util.ArrayList;
import java.util.List;

public class SceneDto extends BaseDto {

	private static final long serialVersionUID = 1L;
	
	private String time;
	private Long zoneId;
	private String zoneName;
	private Long projectId;
	private List<SceneDeviceDto> devices;

	public SceneDto() {
		devices = new ArrayList<>();
	}

	public String getTime() {
		return time;
	}

	public void setTime(String time) {
		this.time = time;
	}

	public Long getZoneId() {
		return zoneId;
	}
	public void setZoneId(Long zoneId) {
		this.zoneId = zoneId;
	}

	public String getZoneName() {
		return zoneName;
	}
	public void setZoneName(String zoneName) {
		this.zoneName = zoneName;
	}

	public List<SceneDeviceDto> getDevices() {
		return devices;
	}
	
	public void setDevices(List<SceneDeviceDto> devices) {
		this.devices = devices;
	}

	public Long getProjectId() {
		return projectId;
	}

	public void setProjectId(Long projectId) {
		this.projectId = projectId;
	}
}
