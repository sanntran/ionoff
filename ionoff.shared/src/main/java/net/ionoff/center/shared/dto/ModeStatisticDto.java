package net.ionoff.center.shared.dto;

import java.io.Serializable;

public class ModeStatisticDto implements Serializable {

	private static final long serialVersionUID = 1L;

	private int totalCount;
	private String activatedName;

	public int getTotalCount() {
		return totalCount;
	}

	public void setTotalCount(int totalCount) {
		this.totalCount = totalCount;
	}

	public String getActivatedName() {
		return activatedName;
	}

	public void setActivatedName(String activatedName) {
		this.activatedName = activatedName;
	}
}
