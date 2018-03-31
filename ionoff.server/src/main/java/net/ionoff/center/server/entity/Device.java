package net.ionoff.center.server.entity;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Set;

public class Device extends BaseObj implements ISlice, Comparable<Device> {

	private static final long serialVersionUID = 1L;

	public static final String PLAYER = "player";
	public static final String LIGHT = "light";
	public static final String APPLIANCE = "appliance";
	
	private Long version;
	private Date time;
	private Integer order;
	private Project project;
	private Zone zone;
	private Set<Relay> relays;

	public Long getVersion() {
		return version;
	}

	public void setVersion(Long version) {
		this.version = version;
	}

	public Boolean getStatus() {
		if (hasOneRelay()) {
			Relay relay =  relays.iterator().next();
			if (relay.isSwitch()) {
				return relay.getStatus();
			}
		}
		return null; // unknown
	}
	
	public Date getTime() {
		return time;
	}
	public void setTime(Date time) {
		this.time = time;
	}

	public Integer getOrder() {
		return order;
	}

	public void setOrder(Integer order) {
		this.order = order;
	}

	public Project getProject() {
		return project;
	}
	public void setProject(Project project) {
		this.project = project;
	}

	public Zone getZone() {
		return zone;
	}
	public void setZone(Zone zone) {
		this.zone = zone;
	}

	public Set<Relay> getRelays() {
		return relays;
	}
	public void setRelays(Set<Relay> relays) {
		this.relays = relays;
	}
	
	public List<Relay> getRelayList() {
		List<Relay> relayList = new ArrayList<>();
		if (relays == null || relays.isEmpty()) {
			return relayList;
		}
		relayList.addAll(relays);
		Collections.sort(relayList, new Comparator<Relay>() {
			@Override
			public int compare(Relay r1, Relay r2) {
				return r1.getNameId().compareTo(r2.getNameId());
			}
		});
		
		return relayList;
	}

	public boolean hasRelay() {
		return relays != null && relays.size() > 0;
	}

	public boolean hasRelays(Relay relay) {
		if (!hasRelay()) {
			return false;
		}
		for (Relay r : relays) {
			if (r.getId() == relay.getId()) {
				return true;
			}
		}
		return false;
	}

	public boolean isAbleToTurnOn() {
		return hasOneRelay() && relays.iterator().next().isSwitch();
	}
	
	private boolean hasOneRelay() {
		return relays != null && relays.size() == 1;
	}

	public boolean isAbleToTurnOff() {
		return hasOneRelay() && relays.iterator().next().isSwitch();
	}

	@Override
	public int compareTo(Device device) {
		Zone zone1 = getZone();
		Zone zone2 = device.getZone();
		
		if ((zone1.getId() != zone2.getId())) {
			return getZone().compareTo(device.getZone());
		}
		if (getOrder() == null) {
			if (device.getOrder() == null) {
				return getName().compareTo(device.getName());
			}
			else {
				return -1;
			}
		}
		if (device.getOrder() == null) {
			return 1;
		}
		return getOrder().compareTo(device.getOrder());
	}

	@Override
	public String toString() {
		StringBuilder sBuilder = new StringBuilder();
		sBuilder.append(super.toString())
				.append(", Clazz: ").append(getClass().getSimpleName());
		return sBuilder.toString();
		
	}
	
	public boolean instanceOf( Class<? extends Device> clazz) {
		return clazz.getSimpleName().equals(getClass().getSimpleName());
	}
}
