package net.ionoff.center.server.entity;

import java.util.Date;
import java.util.Set;

public class Mode extends BaseObj {

	private static final long serialVersionUID = 1L;
	private Integer order;
	private Date time;
	private Boolean isScheduled;
	private String scheduleRepeat;
	private String scheduleTime;
	private String scheduleDay;
	private Boolean isActivated;
	private Project project;
	private Set<ModeScene> scenes;
	private Set<ModeSensor> sensors;

	public Integer getOrder() {
		return order;
	}
	public void setOrder(Integer order) {
		this.order = order;
	}
	
	public Date getTime() {
		return time;
	}
	public void setTime(Date time) {
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
	
	public Boolean getIsActivated() {
		return isActivated;
	}
	public void setIsActivated(Boolean isActivated) {
		this.isActivated = isActivated;
	}
	public Project getProject() {
		return project;
	}	
	public void setProject(Project project) {
		this.project = project;
	}
	
	public Set<ModeScene> getScenes() {
		return scenes;
	}
	public void setScenes(Set<ModeScene> scenes) {
		this.scenes = scenes;
	}
	
	public Set<ModeSensor> getSensors() {
		return sensors;
	}
	
	public void setSensors(Set<ModeSensor> sensors) {
		this.sensors = sensors;
	}
	
	public boolean hasSensor() {
		return sensors != null && !sensors.isEmpty();
	}
	
	public Long getActivatedTime() {
		if (time == null) {
			return 0L;
		}
		return time.getTime();
	}
}
