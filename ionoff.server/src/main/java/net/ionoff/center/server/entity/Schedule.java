package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.Set;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Schedule extends Trigger {

	private static final long serialVersionUID = 1L;

	private Integer order;
	private Boolean enabled;
	private String repeat;
	private String time;
	private String day;
	private Boolean status;
	private Integer retry;
	private Long executedTime;
	private Device device;
	private Project project;
	private Set<ScheduleAction> actions;
}
