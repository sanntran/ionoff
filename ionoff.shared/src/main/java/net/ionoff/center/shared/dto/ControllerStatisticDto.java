package net.ionoff.center.shared.dto;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;

public class ControllerStatisticDto implements Serializable {

	private static final long serialVersionUID = 1L;
	private List<ControllerDto> offline;


	public int getOfflineCount() {
		return offline == null ? 0 : offline.size();
	}

	public Optional<ControllerDto> getFirstOffline() {
		return offline == null || offline.isEmpty() ? Optional.empty() : Optional.of(offline.get(0));
	}

	public List<ControllerDto> getOffline() {
		return offline;
	}

	public void setOffline(List<ControllerDto> offline) {
		this.offline = offline;
	}
}
