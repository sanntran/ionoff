package net.ionoff.center.shared.dto;

import java.io.Serializable;

public class ServerStatisticDto implements Serializable {

	private static final long serialVersionUID = 1L;

	private int memoryUsedPercent;
	private int diskSpaceUsedPercent;

	public int getMemoryUsedPercent() {
		return memoryUsedPercent;
	}

	public void setMemoryUsedPercent(int memoryUsedPercent) {
		this.memoryUsedPercent = memoryUsedPercent;
	}

	public int getDiskSpaceUsedPercent() {
		return diskSpaceUsedPercent;
	}

	public void setDiskSpaceUsedPercent(int diskSpaceUsedPercent) {
		this.diskSpaceUsedPercent = diskSpaceUsedPercent;
	}
}
