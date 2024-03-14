package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Area implements IEntity, Comparable<Area> {

	private static final long serialVersionUID = 1L;

	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private Integer order;
	private Project project;
	private List<Zone> zones;
	
	@Override
	public int compareTo(Area area) {
		if (getOrder() == null) {
			if (area.getOrder() == null) {
				return getNameId().compareTo(area.getNameId());
			}
			else {
				return -1;
			}
		}
		if (area.getOrder() != null) {
			return getOrder().compareTo(area.getOrder());
		}
		else {
			return -1;
		}
	}

	@Override
	public String toString() {
		return "Id: " + getId() + ", Name: " + getName();
	}
}
