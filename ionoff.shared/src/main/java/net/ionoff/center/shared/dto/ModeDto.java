package net.ionoff.center.shared.dto;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ModeDto extends BaseDto {

	private static final long serialVersionUID = 1L;

	private String time;
	private Integer order;
	private Boolean isScheduled;
	private String scheduleRepeat;
	private String scheduleTime;
	private String scheduleDay;
	private Boolean isActivated;
	private Long projectId;
	private String projectName;
	private List<ModeSceneDto> scenes;

	public ModeDto() {
		scenes = new ArrayList<>();
	}

	public String getTime() {
		return time;
	}

	public void setTime(String time) {
		this.time = time;
	}

	public Integer getOrder() {
		return order;
	}

	public void setOrder(Integer order) {
		this.order = order;
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

	public Boolean getIsActivated() {
		return isActivated;
	}

	public void setIsActivated(Boolean isActivated) {
		this.isActivated = isActivated;
	}

	public String getProjectName() {
		return projectName;
	}

	public void setProjectName(String projectName) {
		this.projectName = projectName;
	}
}
