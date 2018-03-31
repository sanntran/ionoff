package net.ionoff.center.server.entity;

import java.util.List;

public class RelayGroup extends BaseObj {

	private static final long serialVersionUID = 1L;
	
	private Project project;
	private List<Relay> relays;

	public List<Relay> getRelays() {
		return relays;
	}

	public void setRelays(List<Relay> relays) {
		this.relays = relays;
	}

	public Project getProject() {
		return project;
	}

	public void setProject(Project project) {
		this.project = project;
	}
	
}