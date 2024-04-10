package net.ionoff.center.shared.dto;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;

public class ControllerStatisticDto implements Serializable {

	private static final long serialVersionUID = 1L;
	private int offlineCount;
	private ControllerDto firstOffline;

	public int getOfflineCount() {
		return offlineCount;
	}

	public void setOfflineCount(int offlineCount) {
		this.offlineCount = offlineCount;
	}

	public ControllerDto getFirstOffline() {
		return firstOffline;
	}

	public void setFirstOffline(ControllerDto firstOffline) {
		this.firstOffline = firstOffline;
	}
}
