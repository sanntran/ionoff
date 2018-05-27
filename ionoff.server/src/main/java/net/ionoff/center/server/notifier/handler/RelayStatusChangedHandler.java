package net.ionoff.center.server.notifier.handler;

import java.util.List;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;

import net.ionoff.center.server.control.IControlService;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.RelayGroup;
import net.ionoff.center.server.entity.RelayGroupRelay;
import net.ionoff.center.server.persistence.dao.IRelayGroupDao;

public class RelayStatusChangedHandler {
	
	private static Logger logger = Logger.getLogger(RelayStatusChangedHandler.class.getName());

	@Autowired
	private IControlService controlService;
	
	@Autowired
	private IRelayGroupDao relayGroupDao;
	
	@Async
	public void onRelayStatusChanged(Relay relay) {
		if (relay == null || relay.getGroupRelays() == null || relay.getGroupRelays().isEmpty()) {
			return;
		}
		
		List<RelayGroup> relayGroups = relayGroupDao.findByRelayId(relay.getId());
		if (relayGroups.isEmpty()) {
			return;
		}
		
		logger.info("Relay " + relay.getSId() + " status changed. Synchronizing relay group state");

		for (RelayGroup relayGroup : relayGroups) {
			List<RelayGroupRelay> groupRelays = relayGroup.getGroupRelays();
			if (groupRelays == null || groupRelays.isEmpty()) {
				break;
			}

			boolean groupHasLeader = false;
			boolean isLeaderOfGroup = false;

			if (relayGroup.hasLeader()) {
				groupHasLeader = true;
				for (RelayGroupRelay groupRelay : groupRelays) {
					if (groupRelay.getRelay().getId() == relay.getId() && Boolean.TRUE.equals(groupRelay.getIsLeader())) {
						isLeaderOfGroup = true;
						break;
					}
				}
			}

			if (!groupHasLeader || isLeaderOfGroup) {
				for (RelayGroupRelay groupRelay : groupRelays) {
					Relay r = groupRelay.getRelay();
					if (r.getId() != relay.getId()) {
						controlService.setRelayState(r, relay.getStatus());
					}
				}
			}
		}
	}
}
