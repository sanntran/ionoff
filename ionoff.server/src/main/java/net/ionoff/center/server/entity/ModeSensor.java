package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.Set;

@Getter
@Setter
public class ModeSensor extends Trigger {

	private static final long serialVersionUID = 1L;
	public static final String CONDITION_VARIABLE = "x";
	
	private Boolean enabled;
	private Integer timeBuffer;
	private Long resetTime;
	private String condition;
	private String message;
	private Mode mode;
	private Sensor sensor;
	private Set<ModeSensorScene> scenes;
	private Set<ModeSensorUser> users;
	private List<Action> actions;
	
	public Boolean getEnabled() {
		return enabled;
	}
	public void setEnabled(Boolean enabled) {
		this.enabled = enabled;
	}
	
	public Integer getTimeBuffer() {
		return timeBuffer;
	}
	public void setTimeBuffer(Integer timeBuffer) {
		this.timeBuffer = timeBuffer;
	}
	
	public Long getResetTime() {
		return resetTime;
	}
	public void setResetTime(Long resetTime) {
		this.resetTime = resetTime;
	}

	public String getCondition() {
		return condition;
	}
	public void setCondition(String condition) {
		this.condition = condition;
	}
	
	public Mode getMode() {
		return mode;
	}
	public void setMode(Mode mode) {
		this.mode = mode;
	}
	
	public Sensor getSensor() {
		return sensor;
	}
	public void setSensor(Sensor sensor) {
		this.sensor = sensor;
	}
	
	public Set<ModeSensorScene> getScenes() {
		return scenes;
	}
	public void setScenes(Set<ModeSensorScene> scenes) {
		this.scenes = scenes;
	}
	
	public boolean hasResetTime() {
		return resetTime != null && resetTime != 0;
	}
	public boolean izEnabled() {
		return enabled != null && enabled == true;
	}
	public Set<ModeSensorUser> getUsers() {
		return users;
	}
	public void setUsers(Set<ModeSensorUser> users) {
		this.users = users;
	}
	public boolean hasScene() {
		return scenes != null && !scenes.isEmpty();
	}
	public boolean hasUser() {
		return users != null && !users.isEmpty();
	}
	public String getMessage() {
		return message;
	}
	public void setMessage(String message) {
		this.message = message;
	}
	
	@Override
	public String toString() {
		return new StringBuilder().append("[ModeSensor] sensor: ").append(sensor.getNameId()).append(", mode: ")
				.append(mode != null ? mode.getNameId() : "null").toString();
	}

	public List<Action> getActions() {
		return actions;
	}

	public void setActions(List<Action> actions) {
		this.actions = actions;
	}
}
