package net.ionoff.center.shared.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class DeviceStatisticDto implements Serializable {

	private static final long serialVersionUID = 1L;

	private int onCount;
	private int offCount;
	private int totalCount;

	public int getOnCount() {
		return onCount;
	}

	public void setOnCount(int onCount) {
		this.onCount = onCount;
	}

	public int getOffCount() {
		return offCount;
	}

	public void setOffCount(int offCount) {
		this.offCount = offCount;
	}

	public int getTotalCount() {
		return totalCount;
	}

	public void setTotalCount(int totalCount) {
		this.totalCount = totalCount;
	}
}
