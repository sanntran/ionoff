package net.ionoff.center.shared.dto;

import java.io.Serializable;

public class ServerInfoDto implements Serializable {

	private static final long serialVersionUID = 1L;
	
	private String serverDate;	
	private String serverTime;	
	private Double freeMemory;	
	private Double totalMemory;	
	private int memoryUsedPercent;
	private Double freeDiskSpace;
	private Double totalDiskSpace;
	private int diskSpaceUsedPercent;
	
	public String getServerDate() {
		return serverDate;
	}
	public void setServerDate(String serverDate) {
		this.serverDate = serverDate;
	}
	
	public String getServerTime() {
		return serverTime;
	}
	public void setServerTime(String serverTime) {
		this.serverTime = serverTime;
	}
	
	public Double getFreeMemory() {
		return freeMemory;
	}
	public void setFreeMemory(Double freeMemory) {
		this.freeMemory = freeMemory;
	}
	
	public Double getTotalMemory() {
		return totalMemory;
	}
	public void setTotalMemory(Double totalMemory) {
		this.totalMemory = totalMemory;
	}
	
	public Double getFreeDiskSpace() {
		return freeDiskSpace;
	}
	public void setFreeDiskSpace(Double freeDiskSpace) {
		this.freeDiskSpace = freeDiskSpace;
	}
	
	public Double getTotalDiskSpace() {
		return totalDiskSpace;
	}
	public void setTotalDiskSpace(Double totalDiskSpace) {
		this.totalDiskSpace = totalDiskSpace;
	}
	
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

	public long calculateUsedDiskSpace() {
		return (long) (totalDiskSpace - freeDiskSpace);
	}
	
	public long calculateUsedMemSpace() {
		return (long)(totalMemory - freeMemory);
	}
}
