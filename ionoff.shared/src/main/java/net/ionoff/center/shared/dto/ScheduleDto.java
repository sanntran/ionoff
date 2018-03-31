package net.ionoff.center.shared.dto;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ScheduleDto extends BaseDto {

	private static final long serialVersionUID = 1L;

	private Boolean enabled;
	private String repeat;
	private String time;
	private String day;
	private Long deviceId;
	private String deviceName;
	private Long projectId;
	private List<ScheduleActionDto> actions;

	public ScheduleDto() {
		actions = new ArrayList<>();
	}

	public Boolean getEnabled() {
		return enabled;
	}
	public void setEnabled(Boolean enabled) {
		this.enabled = enabled;
	}

	public String getRepeat() {
		return repeat;
	}
	public void setRepeat(String repeat) {
		this.repeat = repeat;
	}

	public String getTime() {
		return time;
	}
	public void setTime(String time) {
		this.time = time;
	}

	public String getDay() {
		return day;
	}
	public void setDay(String day) {
		this.day = day;
	}

	public Long getDeviceId() {
		return deviceId;
	}
	public void setDeviceId(Long deviceId) {
		this.deviceId = deviceId;
	}

	public String getDeviceName() {
		return deviceName;
	}
	public void setDeviceName(String deviceName) {
		this.deviceName = deviceName;
	}

	public List<ScheduleActionDto> getActions() {
		return this.actions;
	}
	public void setActions(List<ScheduleActionDto> actions) {
		Collections.sort(actions);
		this.actions = actions;
	}
	public Long getProjectId() {
		return projectId;
	}
	public void setProjectId(Long projectId) {
		this.projectId = projectId;
	}
}
