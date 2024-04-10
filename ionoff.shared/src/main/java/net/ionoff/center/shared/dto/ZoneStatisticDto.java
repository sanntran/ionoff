package net.ionoff.center.shared.dto;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;

public class ZoneStatisticDto implements Serializable {
	
	private static final long serialVersionUID = 1L;

	private int havingAlertCount;
	private ZoneDto firstHasAlert;

	public int getHavingAlertCount() {
		return havingAlertCount;
	}

	public void setHavingAlertCount(int havingAlertCount) {
		this.havingAlertCount = havingAlertCount;
	}

	public ZoneDto getFirstHasAlert() {
		return firstHasAlert;
	}

	public void setFirstHasAlert(ZoneDto firstHasAlert) {
		this.firstHasAlert = firstHasAlert;
	}
}
