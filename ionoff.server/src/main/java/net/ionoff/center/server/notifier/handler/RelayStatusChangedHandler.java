package net.ionoff.center.server.notifier.handler;

import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

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

	private Timer timer;
	
	@Autowired
	private IControlService controlService;
	
	@Autowired
	private IRelayGroupDao relayGroupDao;
	
	public RelayStatusChangedHandler() {
		timer = new Timer();
	}
	
	@Async
	public void onRelayStatusChanged(Relay relay) {
		if (relay == null) {
			return;
		}
		if (relay.izAutoRevert() && !relay.izButton() && relay.isClosed()) {
			scheduleRevertRelayState(relay);
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

	private void scheduleRevertRelayState(Relay relay) {
		logger.info("Schedule revert relay " + relay.getNameId() + " state for " + relay.getAutoRevert() + " second");
		timer.schedule(new TimerTask() {
			@Override
			public void run() {
				controlService.setRelayState(relay, false);
			}
		}, relay.getAutoRevert() * 1000);
	}
}
