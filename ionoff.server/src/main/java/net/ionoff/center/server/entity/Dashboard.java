package net.ionoff.center.server.entity;

import java.util.Set;

public class Dashboard extends BaseObj {

	private static final long serialVersionUID = 1L;
	
	private User user;
	private Zone zone;
	private Project project;
	private Set<DashboardDevice> devices;
	private Set<DashboardScene> scenes;

	public User getUser() {
		return user;
	}

	public void setUser(User user) {
		this.user = user;
	}

	public Zone getZone() {
		return zone;
	}

	public void setZone(Zone zone) {
		this.zone = zone;
	}

	public Project getProject() {
		return project;
	}

	public void setProject(Project project) {
		this.project = project;
	}

	public Set<DashboardDevice> getDevices() {
		return devices;
	}

	public void setDevices(Set<DashboardDevice> devices) {
		this.devices = devices;
	}

	public Set<DashboardScene> getScenes() {
		return scenes;
	}

	public void setScenes(Set<DashboardScene> scenes) {
		this.scenes = scenes;
	}

}
