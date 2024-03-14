package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.Date;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Token implements IEntity {
	
	private static final long serialVersionUID = 1L;

	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private String value;
	private Date expiry;
	private User user;

	public boolean isExpired() {
		if (null == this.expiry) {
			return false;
		}
		return this.expiry.getTime() > System.currentTimeMillis();
	}

}
