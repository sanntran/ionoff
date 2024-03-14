package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.Collections;
import java.util.Date;
import java.util.List;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Device implements ISlice {

	private static final long serialVersionUID = 1L;

	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private Boolean status;
	private Long version;
	private Date time;
	private Integer order;
	private Project project;
	private Zone zone;
	private List<Relay> relays;
	
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
		return "Id: " + getId() + ", Name: " + getName() + ", Clazz: " + getClass().getSimpleName();
	}
	
	public boolean instanceOf( Class<? extends Device> clazz) {
		return clazz.getSimpleName().equals(getClass().getSimpleName());
	}

}
