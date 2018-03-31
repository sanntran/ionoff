package net.ionoff.center.shared.dto;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ModeDto extends BaseDto {

	private static final long serialVersionUID = 1L;

	private String time;
	private Boolean isScheduled;
	private String scheduleRepeat;
	private String scheduleTime;
	private String scheduleDay;
	private Boolean isActivated;
	private Long projectId;
	private List<ModeSceneDto> scenes;
	private List<ModeSensorDto> sensors;

	public ModeDto() {
		scenes = new ArrayList<>();
		sensors = new ArrayList<>();
	}

	public String getTime() {
		return time;
	}

	public void setTime(String time) {
		this.time = time;
	}
	
	public Boolean getIsScheduled() {
		return isScheduled;
	}
	public void setIsScheduled(Boolean isScheduled) {
		this.isScheduled = isScheduled;
	}

	public String getScheduleRepeat() {
		return scheduleRepeat;
	}
	public void setScheduleRepeat(String scheduleRepeat) {
		this.scheduleRepeat = scheduleRepeat;
	}

	public String getScheduleTime() {
		return scheduleTime;
	}
	public void setScheduleTime(String scheduleTime) {
		this.scheduleTime = scheduleTime;
	}

	public String getScheduleDay() {
		return scheduleDay;
	}
	public void setScheduleDay(String scheduleDay) {
		this.scheduleDay = scheduleDay;
	}

	public Long getProjectId() {
		return projectId;
	}
	public void setProjectId(Long projectId) {
		this.projectId = projectId;
	}

	public List<ModeSceneDto> getScenes() {
		return scenes;
	}
	public void setScenes(List<ModeSceneDto> scenes) {
		Collections.sort(scenes);
		this.scenes = scenes;
	}
	public boolean hasSensor() {
		return sensors != null && !sensors.isEmpty();
	}

	public List<ModeSensorDto> getSensors() {
		return sensors;
	}
	public void setSensors(List<ModeSensorDto> sensors) {
		Collections.sort(sensors);
		this.sensors = sensors;
	}

	public Boolean getIsActivated() {
		return isActivated;
	}

	public void setIsActivated(Boolean isActivated) {
		this.isActivated = isActivated;
	}
}
