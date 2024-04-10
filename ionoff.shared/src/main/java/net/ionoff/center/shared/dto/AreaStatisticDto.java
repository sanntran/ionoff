package net.ionoff.center.shared.dto;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;

public class AreaStatisticDto implements Serializable {

	private static final long serialVersionUID = 1L;
	private int havingAlertCount;
	private AreaDto firstHasAlert;

	public int getHavingAlertCount() {
		return havingAlertCount;
	}

	public void setHavingAlertCount(int havingAlertCount) {
		this.havingAlertCount = havingAlertCount;
	}

	public AreaDto getFirstHasAlert() {
		return firstHasAlert;
	}

	public void setFirstHasAlert(AreaDto firstHasAlert) {
		this.firstHasAlert = firstHasAlert;
	}
}
