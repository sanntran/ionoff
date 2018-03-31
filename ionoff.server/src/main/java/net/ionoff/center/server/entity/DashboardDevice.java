package net.ionoff.center.server.entity;

public class DashboardDevice extends BaseObj {

	private static final long serialVersionUID = 1L;

	private Dashboard dashboard;
	private Device device;
	private Project project;
	
	public Device getDevice() {
		return device;
	}
	public void setDevice(Device device) {
		this.device = device;
	}
	
	public Dashboard getDashboard() {
		return dashboard;
	}
	public void setDashboard(Dashboard dashboard) {
		this.dashboard = dashboard;
	}
	public Project getProject() {
		return project;
	}
	public void setProject(Project project) {
		this.project = project;
	}
}
