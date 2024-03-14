package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.Set;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Dashboard implements IEntity {

	private static final long serialVersionUID = 1L;

	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private User user;
	private Zone zone;
	private Project project;
	private Set<DashboardDevice> devices;
	private Set<DashboardScene> scenes;

	@Override
	public String toString() {
		return "Id: " + getId() + ", Name: " + getName();
	}
}
