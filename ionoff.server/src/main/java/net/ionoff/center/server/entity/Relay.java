package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Relay implements IEntity {

	private static final long serialVersionUID = 1L;

	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private String label;
	private Date time;
	private Long version;
	private Integer index;
	private Boolean status;
	private Boolean isLocked;
	private Integer autoRevert;
	private Controller driver;
	private Device device;
	
	private List<RelayGroupRelay> groupRelays;

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
		return status != null && status == true;
	}
	
	public boolean isOpened() {
		return status != null && status == false;
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

	public boolean izAutoRevert() {
		return autoRevert != null && autoRevert > 0;
	}
	
	public boolean izButton() {
		return izAutoRevert() && autoRevert == 1;
	}
}
