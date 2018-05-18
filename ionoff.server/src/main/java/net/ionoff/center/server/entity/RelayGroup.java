package net.ionoff.center.server.entity;

import java.util.ArrayList;
import java.util.List;

public class RelayGroup extends BaseObj {

	private static final long serialVersionUID = 1L;
	
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

	public Project getProject() {
		return project;
	}

	public void setProject(Project project) {
		this.project = project;
	}

	public List<RelayGroupRelay> getGroupRelays() {
		return groupRelays;
	}

	public void setGroupRelays(List<RelayGroupRelay> groupRelays) {
		this.groupRelays = groupRelays;
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