package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class DashboardScene implements IEntity {

	private static final long serialVersionUID = 1L;

	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private Dashboard dashboard;
	private Scene scene;
	private Project project;

	@Override
	public String toString() {
		return "Id: " + getId() + ", Name: " + getName();
	}

}
