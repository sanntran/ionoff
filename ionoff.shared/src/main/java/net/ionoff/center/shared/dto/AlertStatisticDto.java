package net.ionoff.center.shared.dto;

import java.io.Serializable;

public class AlertStatisticDto implements Serializable {
	private static final long serialVersionUID = 1L;
	private int totalCount;

	public int getTotalCount() {
		return totalCount;
	}

	public void setTotalCount(int totalCount) {
		this.totalCount = totalCount;
	}
}
