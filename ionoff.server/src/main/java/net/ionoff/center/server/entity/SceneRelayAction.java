package net.ionoff.center.server.entity;

import net.ionoff.center.shared.entity.RelayAction;

public class SceneRelayAction extends SceneAction implements RelayAction {

	private static final long serialVersionUID = 1L;
	
	private Relay relay;
	
	public Relay getRelay() {
		return relay;
	}
	public void setRelay(Relay relay) {
		this.relay = relay;
	}
}
