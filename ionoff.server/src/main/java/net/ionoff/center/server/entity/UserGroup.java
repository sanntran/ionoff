package net.ionoff.center.server.entity;

public class UserGroup extends BaseObj {

	private static final long serialVersionUID = 1L;
	
	private static final String SYSTEM_ADMIN 	= "SystemAdmin";
	private static final String PROJECT_ADMIN 	= "ProjectAdmin";
	private static final String PROJECT_USER 	= "ProjectUser";
	
	public boolean isSystemAdmin() {
		return SYSTEM_ADMIN.equals(getName());
	}
	
	public boolean isProjectAdmin() {
		return PROJECT_ADMIN.equals(getName());
	}
	
	public boolean isProjectUser() {
		return PROJECT_USER.equals(getName());
	}
}