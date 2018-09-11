package net.ionoff.center.server.entity;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class Relay extends BaseObj {

	private static final long serialVersionUID = 1L;
	
	private String label;
	private Date time;
	private Long version;
	private Integer index;
	private Boolean status;
	private Boolean isLocked;
	private Integer autoRevert;
	private RelayDriver driver;
	private Device device;
	
	private List<RelayGroupRelay> groupRelays;

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

	public Boolean getIsLocked() {
		return isLocked;
	}
	public void setIsLocked(Boolean isLocked) {
		this.isLocked = isLocked;
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
		if (device != null && device.hasOneRelay() && !newStatus.equals(device.getStatus())) {
			status = newStatus;
			return true;
		}
		if (status != null && status.equals(newStatus)) {
			return false;
		}
		status = newStatus;
		return true;
	}
	
	public Integer getAutoRevert() {
		return autoRevert;
	}
	public void setAutoRevert(Integer autoRevert) {
		this.autoRevert = autoRevert;
	}
	
	public boolean isClosed() {
		return status != null && status.booleanValue() == true;
	}
	
	public boolean isOpened() {
		return status != null && status.booleanValue() == false;
	}
	
	public List<RelayGroup> getGroups() {
		List<RelayGroup> relayGroups = new ArrayList<>();
		if (groupRelays == null || groupRelays.isEmpty()) {
			return relayGroups;
		}
		for (RelayGroupRelay groupRelay : groupRelays) {
			relayGroups.add(groupRelay.getGroup());
		}
		return relayGroups;
	}
	
	public List<RelayGroupRelay> getGroupRelays() {
		return groupRelays;
	}
	public void setGroupRelays(List<RelayGroupRelay> groupRelays) {
		this.groupRelays = groupRelays;
	}
	
	public boolean izAutoRevert() {
		return autoRevert != null && autoRevert.intValue() > 0;
	}
	
	public boolean izButton() {
		return izAutoRevert() && autoRevert.intValue() == 1;
	}
}
