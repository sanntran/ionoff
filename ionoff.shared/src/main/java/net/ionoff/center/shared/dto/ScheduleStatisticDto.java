package net.ionoff.center.shared.dto;

import java.io.Serializable;

public class ScheduleStatisticDto implements Serializable {

	private static final long serialVersionUID = 1L;

	private int totalCount;
	private String nextScheduleName;
	private String nextScheduleTime;

	public int getTotalCount() {
		return totalCount;
	}

	public void setTotalCount(int totalCount) {
		this.totalCount = totalCount;
	}

	public String getNextScheduleName() {
		return nextScheduleName;
	}

	public void setNextScheduleName(String nextScheduleName) {
		this.nextScheduleName = nextScheduleName;
	}

	public String getNextScheduleTime() {
		return nextScheduleTime;
	}

	public void setNextScheduleTime(String nextScheduleTime) {
		this.nextScheduleTime = nextScheduleTime;
	}
}
