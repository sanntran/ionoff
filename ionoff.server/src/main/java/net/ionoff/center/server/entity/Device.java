package net.ionoff.center.server.entity;

import java.util.Collections;
import java.util.Date;
import java.util.List;

public class Device extends BaseObj implements ISlice {

	private static final long serialVersionUID = 1L;

	private Boolean status;
	private Long version;
	private Date time;
	private Integer order;
	private Project project;
	private Zone zone;
	private List<Relay> relays;

	public Boolean getStatus() {
		return status;
	} 

	public void setStatus(Boolean status) {
		this.status = status;
	}
	
	public Long getVersion() {
		return version;
	}

	public void setVersion(Long version) {
		this.version = version;
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

	public List<Relay> getRelays() {
		return relays;
	}
	public void setRelays(List<Relay> relays) {
		this.relays = relays;
	}
	
	public List<Relay> getRelayList() {
		return relays == null ? Collections.emptyList() : relays;
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

	public boolean hasOneRelay() {
		return relays != null && relays.size() == 1;
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
