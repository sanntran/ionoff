package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class RelayGroup implements IEntity {

	private static final long serialVersionUID = 1L;

	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private Project project;
	private List<RelayGroupRelay> groupRelays;

	public List<Relay> getRelays() {
		 List<Relay> relays = new ArrayList<>();
		 if (groupRelays == null || groupRelays.isEmpty()) {
			 return relays;
		 }
		 for (RelayGroupRelay groupRelay : groupRelays) {
			 relays.add(groupRelay.getRelay());
		 }
		 return relays;
	}
	
	public boolean hasLeader() {
		if (groupRelays == null || groupRelays.isEmpty()) {
			return false;
		}
		for (RelayGroupRelay groupRelay : groupRelays) {
			if (Boolean.TRUE.equals(groupRelay.getIsLeader())) {
				return true;
			}
		}
		return false;
	}
	
}