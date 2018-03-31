package net.ionoff.center.shared.dto;

import java.util.List;

public class UserDto extends BaseDto {

	private static final long serialVersionUID = 1L;
	public static final String PASSWORD_DISPLAY = "*********";
	
	private String password;
	private String fullName;
	private String phoneNo;
	private String email;
	private String groupName;
	private String language;
	private List<ProjectDto> projects;

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getFullName() {
		return this.fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getPhoneNo() {
		return phoneNo;
	}

	public void setPhoneNo(String phoneNo) {
		this.phoneNo = phoneNo;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}

	public String getGroupName() {
		return groupName;
	}

	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}

	public List<ProjectDto> getProjects() {
		return projects;
	}

	public void setProjects(List<ProjectDto> projects) {
		this.projects = projects;
	}
	
}
