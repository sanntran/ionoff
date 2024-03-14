package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.Date;
import java.util.Set;
@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Mode implements IEntity {

	private static final long serialVersionUID = 1L;

	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private Integer order;
	private Date time;
	private Boolean isScheduled;
	private String scheduleRepeat;
	private String scheduleTime;
	private String scheduleDay;
	private Boolean isActivated;
	private Project project;
	private Set<ModeScene> scenes;
	private Set<ModeSensor> sensors;
	
	public boolean hasSensor() {
		return sensors != null && !sensors.isEmpty();
	}
	
	public Long getActivatedTime() {
		if (time == null) {
			return 0L;
		}
		return time.getTime();
	}
}
