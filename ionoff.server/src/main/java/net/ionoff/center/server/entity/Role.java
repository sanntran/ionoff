package net.ionoff.center.server.entity;

import org.springframework.security.core.GrantedAuthority;

public enum Role implements GrantedAuthority {
	
	PROJECT_USER("PROJECT_USER"), PROJECT_ADMIN("PROJECT_ADMIN"), SYSTEM_ADMIN("SYSTEM_ADMIN");

	private String authority;

	Role(String authority) {
		this.authority = authority;
	}

	@Override
	public String getAuthority() {
		return this.authority;
	}
}