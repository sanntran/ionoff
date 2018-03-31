package net.ionoff.center.server.entity;

import java.util.Date;

public class Token extends BaseObj {
	
	private static final long serialVersionUID = 1L;
	
	private String value;
	private Date expiry;
	private User user;

	public User getUser() {
		return user;
	}

	public void setUser(User user) {
		this.user = user;
	}

	public Date getExpiry() {
		return expiry;
	}

	public void setExpiry(Date expiry) {
		this.expiry = expiry;
	}
	
	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public boolean isExpired() {
		if (null == this.expiry) {
			return false;
		}
		return this.expiry.getTime() > System.currentTimeMillis();
	}

}
