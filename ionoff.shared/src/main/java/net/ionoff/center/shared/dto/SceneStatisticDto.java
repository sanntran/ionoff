package net.ionoff.center.shared.dto;

import java.io.Serializable;

public class SceneStatisticDto implements Serializable {

	private static final long serialVersionUID = 1L;

	private int totalCount;
	private String lastPlayedName;

	public int getTotalCount() {
		return totalCount;
	}

	public void setTotalCount(int totalCount) {
		this.totalCount = totalCount;
	}

	public String getLastPlayedName() {
		return lastPlayedName;
	}

	public void setLastPlayedName(String lastPlayedName) {
		this.lastPlayedName = lastPlayedName;
	}
}
