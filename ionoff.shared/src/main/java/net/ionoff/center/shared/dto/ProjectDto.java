package net.ionoff.center.shared.dto;

import java.util.ArrayList;
import java.util.List;

public class ProjectDto extends BaseDto {

	private static final long serialVersionUID = 1L;

	private String address;
	private Long activatedModeId;
	private List<ZoneDto> zones;

	public ProjectDto() {
		zones = new ArrayList<>();
	}

	public String getAddress() {
		return address;
	}

	public void setAddress(String address) {
		this.address = address;
	}

	public List<ZoneDto> getZones() {
		return zones;
	}

	public void setZones(List<ZoneDto> zones) {
		this.zones = zones;
	}

	public Long getActivatedModeId() {
		return activatedModeId;
	}

	public void setActivatedModeId(Long activatedModeId) {
		this.activatedModeId = activatedModeId;
	}
}
