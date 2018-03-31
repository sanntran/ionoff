package net.ionoff.center.server.entity;

public class UserScene extends BaseObj {

	private static final long serialVersionUID = 1L;

	private Boolean role;
	private User user;
	private Scene scene;
	private Project project;
	
	public Boolean getRole() {
		return role;
	}
	public void setRole(Boolean role) {
		this.role = role;
	}
	
	public User getUser() {
		return user;
	}
	public void setUser(User user) {
		this.user = user;
	}
	
	public Scene getScene() {
		return scene;
	}
	public void setScene(Scene scene) {
		this.scene = scene;
	}
	
	public boolean hasRole() {
		return role != null && role.booleanValue() == true;
	}
	public Project getProject() {
		return project;
	}
	public void setProject(Project project) {
		this.project = project;
	}
}
