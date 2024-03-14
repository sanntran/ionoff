package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.Date;
import java.util.List;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Switch implements IEntity {

	private static final long serialVersionUID = 1L;
	public static final int NULL_INPUT = -1;

	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private Integer index;
	private Date time;
	private Boolean status;
	private Controller driver;
	private List<Sensor> sensors;

	public boolean updateStatus(Boolean newStatus) {
		if (newStatus == null) {
			return false;
		}
		if (status != null && status.equals(newStatus)) {
			return false;
		}
		status = newStatus;
		time = new Date();
		return true;
	}
	
}
