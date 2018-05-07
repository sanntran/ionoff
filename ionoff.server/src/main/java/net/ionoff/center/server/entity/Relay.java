package net.ionoff.center.server.entity;

import java.util.Date;

public class Relay extends BaseObj {

	private static final long serialVersionUID = 1L;

	public static final String SWITCH = "Switch";
	public static final String BUTTON = "Button";
	
	private String label;
	private Date time;
	private Long version;
	private String type;
	private Integer index;
	private Boolean status;
	private Boolean isLeader;
	private RelayDriver driver;
	private Device device;
	private RelayGroup group;

	public String getLabel() {
		return label;
	}
	public void setLabel(String label) {
		this.label = label;
	}
	
	public Date getTime() {
		return time;
	}
	public void setTime(Date time) {
		this.time = time;
	}
	public Long getVersion() {
		return version;
	}
	public void setVersion(Long version) {
		this.version = version;
	}
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

	public Boolean getIsLeader() {
		return isLeader;
	}
	public void setIsLeader(Boolean isLeader) {
		this.isLeader = isLeader;
	}
	public RelayDriver getDriver() {
		return driver;
	}
	public void setDriver(RelayDriver relayDriver) {
		this.driver = relayDriver;
	}

	public Device getDevice() {
		return device;
	}
	public void setDevice(Device device) {
		this.device = device;
	}

	public boolean updateStatus(Boolean newStatus) {
		if (newStatus == null) {
			return false;
		}
		if (status != null && status.equals(newStatus)) {
			return false;
		}
		status = newStatus;
		time = new Date();
		return true;
	}
	
	public boolean isClosed() {
		return status != null && status.booleanValue() == true;
	}
	
	public boolean isOpened() {
		return status != null && status.booleanValue() == false;
	}
	
	public boolean isButton() {
		return BUTTON.equals(type);
	}
	
	public boolean isSwitch() {
		return SWITCH.equals(type);
	}
	
	public RelayGroup getGroup() {
		return group;
	}
	public void setGroup(RelayGroup group) {
		this.group = group;
	}
}
