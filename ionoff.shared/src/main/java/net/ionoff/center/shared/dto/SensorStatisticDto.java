package net.ionoff.center.shared.dto;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;

public class SensorStatisticDto implements Serializable {
	
	private static final long serialVersionUID = 1L;
	private List<SensorDto> offline;

	public int getOfflineCount() {
		return offline == null ? 0 : offline.size();
	}

	public Optional<SensorDto> getFirstOffline() {
		return offline == null || offline.isEmpty() ? Optional.empty() : Optional.of(offline.get(0));
	}

	public List<SensorDto> getOffline() {
		return offline;
	}

	public void setOffline(List<SensorDto> offline) {
		this.offline = offline;
	}
}
