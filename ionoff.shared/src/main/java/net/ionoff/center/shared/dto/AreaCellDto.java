package net.ionoff.center.shared.dto;

public class AreaCellDto extends AreaDto {

	private static final long serialVersionUID = 1L;

	private Integer zoneCount;
	private Integer alertCount;

	public Integer getZoneCount() {
		return zoneCount;
	}

	public void setZoneCount(Integer zoneCount) {
		this.zoneCount = zoneCount;
	}

	public Integer getAlertCount() {
		return alertCount;
	}

	public void setAlertCount(Integer alertCount) {
		this.alertCount = alertCount;
	}

	public boolean hasAlert() {
		return alertCount != null && alertCount > 0;
	}
}
