package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.Set;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Project implements IEntity {

	private static final long serialVersionUID = 1L;
	public static final long DEFAULT_ID = 1L;

	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private String address;
	private List<Zone> zones;
	private List<Area> areas;
	private Set<UserProject> users;
	private Set<Mode> modes;
	private Set<Sensor> sensors;
	private Set<Controller> controllers;

	public boolean hasZone() {
		return zones != null && !zones.isEmpty();
	}

	public boolean hasArea() {
		return areas != null && !areas.isEmpty();
	}
	
	public boolean hasMode() {
		return modes != null && !modes.isEmpty();
	}
}
