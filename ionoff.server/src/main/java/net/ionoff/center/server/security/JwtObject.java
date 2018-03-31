package net.ionoff.center.server.security;

import java.util.Date;

public class JwtObject {

	private String user;
	private String hash;
	private Boolean expired;
	private Date created;
	private Date expiration;
	
	public String getUser() {
		return user;
	}
	public void setUser(String user) {
		this.user = user;
	}
	
	public String getHash() {
		return hash;
	}
	public void setHash(String hash) {
		this.hash = hash;
	}
	
	public Date getCreated() {
		return created;
	}
	public void setCreated(Date created) {
		this.created = created;
	}
	
	public Boolean getExpired() {
		return expired;
	}
	public void setExpired(Boolean expired) {
		this.expired = expired;
	}
	
	public Boolean isExpired() {
		return expiration != null && expiration.before(new Date());
	}
	
	public Boolean canBeRefreshed() {
		return !isExpired();
	}
	
	public Date getExpiration() {
		return expiration;
	}
	public void setExpiration(Date expiration) {
		this.expiration = expiration;
	}
	
}