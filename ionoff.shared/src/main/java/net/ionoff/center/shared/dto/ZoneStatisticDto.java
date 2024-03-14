package net.ionoff.center.shared.dto;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;

public class ZoneStatisticDto implements Serializable {
	
	private static final long serialVersionUID = 1L;
	private List<ZoneDto> havingAlerts;

	public int getHavingAlertsCount() {
		return havingAlerts == null ? 0 : havingAlerts.size();
	}

	public Optional<ZoneDto> getFirstHavingAlerts() {
		return havingAlerts == null || havingAlerts.isEmpty() ? Optional.empty() : Optional.of(havingAlerts.get(0));
	}

	public List<ZoneDto> getHavingAlerts() {
		return havingAlerts;
	}

	public void setHavingAlerts(List<ZoneDto> havingAlerts) {
		this.havingAlerts = havingAlerts;
	}
}
