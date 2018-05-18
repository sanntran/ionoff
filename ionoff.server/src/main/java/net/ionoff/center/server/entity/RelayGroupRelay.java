package net.ionoff.center.server.entity;

public class RelayGroupRelay extends BaseObj {

	private static final long serialVersionUID = 1L;
	
	private Boolean isLeader;
	private Relay relay;
	private RelayGroup group;

	public Boolean getIsLeader() {
		return isLeader;
	}

	public void setIsLeader(Boolean isLeader) {
		this.isLeader = isLeader;
	}

	public Relay getRelay() {
		return relay;
	}

	public void setRelay(Relay relay) {
		this.relay = relay;
	}
	
	public RelayGroup getGroup() {
		return group;
	}

	public void setGroup(RelayGroup group) {
		this.group = group;
	}

	
}