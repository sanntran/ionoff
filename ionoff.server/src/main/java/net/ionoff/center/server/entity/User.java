package net.ionoff.center.server.entity;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

public class User extends BaseObj implements UserDetails {
	
	private static final long serialVersionUID = 1L;
	
	public static final String LORD = "lord"; 
	
	private String password;
	private String fullName;
	private String phoneNo;
	private String email;
	private String language;
	private UserGroup group;
	private Set<UserProject> projects;
	private Set<UserZone> zones;

	@Override
	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
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

	public String getFullName() {
		return this.fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}

	public UserGroup getGroup() {
		return group;
	}

	public void setGroup(UserGroup group) {
		this.group = group;
	}

	public Set<UserProject> getProjects() {
		return projects;
	}

	public void setProjects(Set<UserProject> projects) {
		this.projects = projects;
	}
	
	@Override
	public Collection<? extends GrantedAuthority> getAuthorities() {
		Set<Role> roles = new HashSet<Role>();
		if (group.isSystemAdmin()) {
			roles.add(Role.SYSTEM_ADMIN);
			roles.add(Role.PROJECT_ADMIN);
			roles.add(Role.PROJECT_USER);
		}
		else if (group.isProjectAdmin()) {
			roles.add(Role.PROJECT_ADMIN);
			roles.add(Role.PROJECT_USER);
		}
		else if (group.isProjectUser()) {
			roles.add(Role.PROJECT_USER);
		}
		return roles;
	}

	@Override
	public String getUsername() {
		return getName();
	}

	@Override
	public boolean isAccountNonExpired() {
		return true;
	}

	@Override
	public boolean isAccountNonLocked() {
		return true;
	}

	@Override
	public boolean isCredentialsNonExpired() {
		return true;
	}

	@Override
	public boolean isEnabled() {
		return true;
	}

	public Set<UserZone> getZones() {
		return zones;
	}

	public void setZones(Set<UserZone> zones) {
		this.zones = zones;
	}
	
	public boolean hasAdminRole() {
		return group.isProjectAdmin() || group.isSystemAdmin();
	}

	public boolean isProjectAdmin() {
		return group.isProjectAdmin();
	}

	public boolean isSystemAdmin() {
		return group.isSystemAdmin();
	}
}
