package net.ionoff.center.server.entity;

import lombok.Getter;
import lombok.Setter;
import net.ionoff.center.shared.entity.RelayAction;

@Getter
@Setter
public class ScheduleRelayAction extends ScheduleAction implements RelayAction {

	private static final long serialVersionUID = 1L;
	
	private Relay relay;
	
	public Relay getRelay() {
		return relay;
	}
	public void setRelay(Relay relay) {
		this.relay = relay;
	}
}
