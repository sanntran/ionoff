package net.ionoff.center.server.entity;

public class DashboardScene extends BaseObj {

	private static final long serialVersionUID = 1L;

	private Dashboard dashboard;
	private Scene scene;
	private Project project;
	
	public Scene getScene() {
		return scene;
	}
	public void setScene(Scene scene) {
		this.scene = scene;
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
