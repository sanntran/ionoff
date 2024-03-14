package net.ionoff.center.server.entity;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class User implements IEntity, UserDetails {
	
	private static final long serialVersionUID = 1L;
	
	public static final String LORD = "lord";

	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private String password;
	private String fullName;
	private String phoneNo;
	private String email;
	private String language;
	private UserGroup group;
	private Set<UserProject> projects;
	private Set<UserZone> zones;
	
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
