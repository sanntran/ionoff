package net.ionoff.center.server.entity;

import java.util.List;

public class Zone extends BaseObj {

	private static final long serialVersionUID = 1L;
	
	private Integer order;
	private Area area;
	private Project project;
	private List<Device> devices;
	private List<Scene> scenes;
	
	public Integer getOrder() {
		return order;
	}
	public void setOrder(Integer order) {
		this.order = order;
	}
	public Area getArea() {
		return area;
	}	
	public void setArea(Area area) {
		this.area = area;
	}
	
	public Project getProject() {
		return project;
	}	
	public void setProject(Project project) {
		this.project = project;
	}
	
	public List<Device> getDevices() {
		return devices;
	}
	public void setDevices(List<Device> devices) {
		this.devices = devices;
	}

	public boolean hasScene() {
		return scenes != null && !scenes.isEmpty();
	}
	
	public List<Scene> getScenes() {
		return scenes;
	}
	
	public void setScenes(List<Scene> scenes) {
		this.scenes = scenes;
	}

	public boolean hasDevices() {
		return devices != null && !devices.isEmpty();
	}
}
