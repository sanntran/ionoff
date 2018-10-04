package net.ionoff.center.shared.dto;

public class RelayDto extends BaseDto {

	private static final long serialVersionUID = 1L;

	public static final String DRIVER = "relaydriver";
	public static final String DEVICE = "device";

	private String time;
	private Integer index;
	private Boolean status;
	private String label;
	private Boolean isLocked;
	private Boolean isLeader;
	private Integer autoRevert;
	private Long driverId;
	private String driverName;
	private Long deviceId;
	private String deviceName;

	public Integer getIndex() {
		return index;
	}
	public void setIndex(Integer index) {
		this.index = index;
	}

	public Boolean getStatus() {
		return status;
	}
	public void setStatus(Boolean status) {
		this.status = status;
	}

	public Integer getAutoRevert() {
		return autoRevert;
	}
	public void setAutoRevert(Integer autoRevert) {
		this.autoRevert = autoRevert;
	}
	
	public Long getDriverId() {
		return driverId;
	}
	public void setDriverId(Long driverId) {
		this.driverId = driverId;
	}

	public String getDriverName() {
		return driverName;
	}
	public void setDriverName(String driverName) {
		this.driverName = driverName;
	}

	public Long getDeviceId() {
		return deviceId;
	}
	public void setDeviceId(Long deviceId) {
		this.deviceId = deviceId;
	}

	public String getDeviceName() {
		return deviceName;
	}
	public void setDeviceName(String deviceName) {
		this.deviceName = deviceName;
	}

	public String getLabel() {
		return label;
	}
	public void setLabel(String label) {
		this.label = label;
	}
	
	public String getTime() {
		if (time == null) {
			return "---";
		}
		return time;
	}
	public void setTime(String time) {
		this.time = time;
	}
	
	public Boolean getIsLeader() {
		return isLeader;
	}
	public void setIsLeader(Boolean isLeader) {
		this.isLeader = isLeader;
	}
	public Boolean getIsLocked() {
		return isLocked;
	}
	public void setIsLocked(Boolean isLocked) {
		this.isLocked = isLocked;
	}
	
	public boolean izAutoRevert() {
		return autoRevert != null && autoRevert > 0;
	}
}
