package net.ionoff.center.server.notifier.handler;

import java.util.List;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;

import net.ionoff.center.server.control.IControlService;
import net.ionoff.center.server.entity.Relay;

public class RelayStatusChangedHandler {
	
	private static Logger logger = Logger.getLogger(RelayStatusChangedHandler.class.getName());

	@Autowired
	private IControlService controlService;
	
	@Async
	public void onRelayStatusChanged(Relay relay) {
		if (relay != null && relay.getGroup() != null) {
			logger.info("Relay " + relay.getSId() + " status changed. Synchronizing relay group state");
			List<Relay> relays = relay.getGroup().getRelays();
			if (hasLeader(relays)) {
				if (!Boolean.TRUE.equals(relay.getIsLeader())) {
					return;
				}
			}
			for (Relay r : relays) {
				if (r.getId() != relay.getId()) {
					controlService.setRelayState(r, relay.getStatus());
				}
			}
		}
	}
	
	private boolean hasLeader(List<Relay> relays) {
		for (Relay r : relays) {
			if (Boolean.TRUE.equals(r.getIsLeader())) {
				return true;
			}
		}
		return false;
	}
}
