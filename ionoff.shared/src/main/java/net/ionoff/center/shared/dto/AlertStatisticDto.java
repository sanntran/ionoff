package net.ionoff.center.shared.dto;

import java.io.Serializable;

public class AlertStatisticDto implements Serializable {
	private static final long serialVersionUID = 1L;
	private int totalCount;
	private SensorDto firstAlert;

	public int getTotalCount() {
		return totalCount;
	}

	public void setTotalCount(int totalCount) {
		this.totalCount = totalCount;
	}

	public SensorDto getFirstAlert() {
		return firstAlert;
	}

	public void setFirstAlert(SensorDto firstAlert) {
		this.firstAlert = firstAlert;
	}
}
