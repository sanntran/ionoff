package net.ionoff.center.shared.dto;

import java.io.Serializable;

public class ControllerStatisticDto implements Serializable {

	private static final long serialVersionUID = 1L;

	private int onlineCount;
	private int offlineCount;
	private int totalCount;

	public int getOnlineCount() {
		return onlineCount;
	}

	public void setOnlineCount(int onlineCount) {
		this.onlineCount = onlineCount;
	}

	public int getOfflineCount() {
		return offlineCount;
	}

	public void setOfflineCount(int offlineCount) {
		this.offlineCount = offlineCount;
	}

	public int getTotalCount() {
		return totalCount;
	}

	public void setTotalCount(int totalCount) {
		this.totalCount = totalCount;
	}
}
