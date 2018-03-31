package net.ionoff.center.shared.dto;

import java.util.ArrayList;
import java.util.List;

public class AreaDto extends BaseDto {

	private static final long serialVersionUID = 1L;
	public static final String ORDER = "order";
	
	private Integer order;
	private Long projectId;
	private List<ZoneDto> zones;

	public Integer getOrder() {
		return order;
	}

	public void setOrder(Integer order) {
		this.order = order;
	}

	public AreaDto() {
		zones = new ArrayList<>();
	}

	public Long getProjectId() {
		return projectId;
	}

	public void setProjectId(Long projectId) {
		this.projectId = projectId;
	}

	public List<ZoneDto> getZones() {
		return zones;
	}

	public boolean hasZone() {
		return zones != null && !zones.isEmpty();
	}

	public void setZones(List<ZoneDto> zones) {
		this.zones = zones;
	}
}
