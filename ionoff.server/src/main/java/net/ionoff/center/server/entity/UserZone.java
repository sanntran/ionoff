package net.ionoff.center.server.entity;

public class UserZone extends BaseObj {

	private static final long serialVersionUID = 1L;

	private Boolean role;
	private User user;
	private Zone zone;
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
	
	public Zone getZone() {
		return zone;
	}
	public void setZone(Zone zone) {
		this.zone = zone;
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
