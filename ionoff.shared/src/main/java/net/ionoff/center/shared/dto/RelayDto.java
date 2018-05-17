package net.ionoff.center.shared.dto;

public class RelayDto extends BaseDto {

	private static final long serialVersionUID = 1L;

	public static final String SWITCH = "Switch";
	public static final String BUTTON = "Button";

	public static final String DRIVER = "driver";
	public static final String DEVICE = "device";

	private String type;
	private String time;
	private Integer index;
	private Boolean status;
	private String label;
	private Boolean isLeader;
	private Long driverId;
	private String driverName;
	private Long deviceId;
	private String deviceName;

	public String getType() {
		return type;
	}
	public void setType(String type) {
		this.type = type;
	}

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

	public boolean isButton() {
		return BUTTON.equals(type);
	}
	
	public boolean isSwitch() {
		return SWITCH.equals(type);
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
}
