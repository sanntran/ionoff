package net.ionoff.center.shared.dto;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;

public class AreaStatisticDto implements Serializable {

	private static final long serialVersionUID = 1L;
	private List<AreaDto> havingAlerts;

	public int getHavingAlertsCount() {
		return havingAlerts == null ? 0 : havingAlerts.size();
	}

	public Optional<AreaDto> getFirstHavingAlerts() {
		return havingAlerts == null || havingAlerts.isEmpty() ? Optional.empty() : Optional.of(havingAlerts.get(0));
	}

	public List<AreaDto> getHavingAlerts() {
		return havingAlerts;
	}

	public void setHavingAlerts(List<AreaDto> havingAlerts) {
		this.havingAlerts = havingAlerts;
	}
}
