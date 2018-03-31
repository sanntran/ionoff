package net.ionoff.center.server.entity;

import java.util.List;

public class Area extends BaseObj implements Comparable<Area> {

	private static final long serialVersionUID = 1L;
	
	private Integer order;
	private Project project;
	private List<Zone> zones;
	
	public Integer getOrder() {
		return order;
	}
	public void setOrder(Integer order) {
		this.order = order;
	}
	public Project getProject() {
		return project;
	}
	public void setProject(Project project) {
		this.project = project;
	}
	public List<Zone> getZones() {
		return zones;
	}
	public void setZones(List<Zone> zones) {
		this.zones = zones;
	}
	
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
		StringBuilder builder = new StringBuilder();
		
		builder.append("Id: ").append(getId())
				.append(", Name: " + getName());
		
		return builder.toString();
	}
}
