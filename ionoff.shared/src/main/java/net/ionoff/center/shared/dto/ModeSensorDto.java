package net.ionoff.center.shared.dto;

import java.util.List;

public class ModeSensorDto extends BaseDto {

	private static final long serialVersionUID = 1L;
	
	private Boolean enabled;
	private String condition;
	private Long modeId;
	private String modeName;
	private Long sensorId;
	private String sensorName;
	private List<ModeSensorSceneDto> scenes;
	private List<ModeSensorUserDto> users;
	
	public Boolean getEnabled() {
		return enabled != null ? enabled : false;
	}
	public void setEnabled(Boolean enabled) {
		this.enabled = enabled;
	}
	
	public String getCondition() {
		return condition;
	}
	public void setCondition(String condition) {
		this.condition = condition;
	}
	public Long getModeId() {
		return modeId;
	}
	public void setModeId(Long modeId) {
		this.modeId = modeId;
	}
	
	public String getModeName() {
		return modeName;
	}
	public void setModeName(String modeName) {
		this.modeName = modeName;
	}
	
	public Long getSensorId() {
		return sensorId;
	}
	public void setSensorId(Long sensorId) {
		this.sensorId = sensorId;
	}
	
	public String getSensorName() {
		return sensorName;
	}
	public void setSensorName(String sensorName) {
		this.sensorName = sensorName;
	}
	
	public List<ModeSensorSceneDto> getScenes() {
		return scenes;
	}
	public void setScenes(List<ModeSensorSceneDto> scenes) {
		this.scenes = scenes;
	}
	
	public List<ModeSensorUserDto> getUsers() {
		return users;
	}
	public void setUsers(List<ModeSensorUserDto> users) {
		this.users = users;
	}
}
