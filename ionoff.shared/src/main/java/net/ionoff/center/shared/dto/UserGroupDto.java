package net.ionoff.center.shared.dto;

public class UserGroupDto extends BaseDto {
	
	private static final long serialVersionUID = 1L;
	public static final String PROJECT_USER = "ProjectUser";
	public static final String PROJECT_ADMIN = "ProjectAdmin";
	public static final String SYSTEM_ADMIN = "SystemAdmin";
	
	public static boolean isProjectAdmin(String groupName) {
		return PROJECT_ADMIN.equals(groupName);
	}
	
	public static boolean isSystemAdmin(String groupName) {
		return SYSTEM_ADMIN.equals(groupName);
	}
	
	public static boolean isProjectUser(String groupName) {
		return PROJECT_USER.equals(groupName);
	}
}