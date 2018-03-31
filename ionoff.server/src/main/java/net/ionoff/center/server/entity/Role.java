package net.ionoff.center.server.entity;

import org.springframework.security.core.GrantedAuthority;

public enum Role implements GrantedAuthority {
	
	ROLE_PROJECT_USER("ROLE_PROJECT_USER"), ROLE_PROJECT_ADMIN("ROLE_PROJECT_ADMIN"), ROLE_SYSTEM_ADMIN("ROLE_SYSTEM_ADMIN");

	private String authority;

	Role(String authority) {
		this.authority = authority;
	}

	@Override
	public String getAuthority() {
		return this.authority;
	}
}