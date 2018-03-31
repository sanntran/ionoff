package net.ionoff.center.shared.dto;

import java.util.List;

public class UserZoneDto extends BaseDto {

	private static final long serialVersionUID = 1L;
	
	private Boolean role;
	private Long userId;
	private String userName;
	private Long zoneId;
	private String zoneName;
	private Long areaId;
	private String areaName;
	private Long projectId;
	private List<UserSceneDto> userScenes;
	private List<UserDeviceDto> userDevices;
	
	public Long getUserId() {
		return userId;
	}
	public void setUserId(Long userId) {
		this.userId = userId;
	}
	
	public String getUserName() {
		return userName;
	}
	public void setUserName(String userName) {
		this.userName = userName;
	}
	
	public Boolean getRole() {
		return role;
	}
	public void setRole(Boolean role) {
		this.role = role;
	}
	
	public boolean hasRole() {
		return Boolean.TRUE.equals(role);
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
	
	public Long getAreaId() {
		return areaId;
	}
	public void setAreaId(Long areaId) {
		this.areaId = areaId;
	}
	
	public String getAreaName() {
		return areaName;
	}
	public void setAreaName(String areaName) {
		this.areaName = areaName;
	}
	
	public Long getProjectId() {
		return projectId;
	}
	public void setProjectId(Long projectId) {
		this.projectId = projectId;
	}
	
	public List<UserDeviceDto> getUserDevices() {
		return userDevices;
	}
	public void setUserDevices(List<UserDeviceDto> userDevices) {
		this.userDevices = userDevices;
	}
	
	public List<UserSceneDto> getUserScenes() {
		return userScenes;
	}
	public void setUserScenes(List<UserSceneDto> userScenes) {
		this.userScenes = userScenes;
	}
}
