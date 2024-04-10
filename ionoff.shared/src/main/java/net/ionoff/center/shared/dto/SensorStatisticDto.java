package net.ionoff.center.shared.dto;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;

public class SensorStatisticDto implements Serializable {
	
	private static final long serialVersionUID = 1L;
	private int offlineCount;
	private SensorDto firstOffline;

	public int getOfflineCount() {
		return offlineCount;
	}

	public void setOfflineCount(int offlineCount) {
		this.offlineCount = offlineCount;
	}

	public SensorDto getFirstOffline() {
		return firstOffline;
	}

	public void setFirstOffline(SensorDto firstOffline) {
		this.firstOffline = firstOffline;
	}
}
