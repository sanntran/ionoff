package net.ionoff.center.server.entity;

import java.util.Set;

public class Schedule extends BaseObj {

	private static final long serialVersionUID = 1L;

	private Integer order;
	private Boolean enabled;
	private String repeat;
	private String time;
	private String day;
	private Boolean status;
	private Integer retry;
	private Long executedTime;
	private Device device;
	private Project project;
	private Set<ScheduleAction> actions;

	public Integer getOrder() {
		return order;
	}

	public void setOrder(Integer order) {
		this.order = order;
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
	
	public Boolean getStatus() {
		return status;
	}
	public void setStatus(Boolean status) {
		this.status = status;
	}
	
	public Integer getRetry() {
		return retry;
	}
	public void setRetry(Integer retry) {
		this.retry = retry;
	}
	
	public Device getDevice() {
		return device;
	}
	public void setDevice(Device device) {
		this.device = device;
	}
	
	public Long getExecutedTime() {
		return executedTime;
	}
	public void setExecutedTime(Long executedTime) {
		this.executedTime = executedTime;
	}
	public Set<ScheduleAction> getActions() {
		return actions;
	}
	public void setActions(Set<ScheduleAction> actions) {
		this.actions = actions;
	}
	
	public Project getProject() {
		return project;
	}
	public void setProject(Project project) {
		this.project = project;
	}
}
