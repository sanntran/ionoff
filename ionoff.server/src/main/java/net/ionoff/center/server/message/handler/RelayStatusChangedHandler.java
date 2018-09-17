package net.ionoff.center.server.message.handler;

import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

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

	private static final ScheduledExecutorService SCHEDULER_EXECUTOR = Executors.newScheduledThreadPool(1);

	@Autowired
	private IControlService controlService;
	
	@Autowired
	private IRelayGroupDao relayGroupDao;

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
		SCHEDULER_EXECUTOR.schedule(new Runnable() {
			@Override
			public void run() {
				logger.info("Execute revert relay " + relay.getNameId() + " after " + relay.getAutoRevert() + " second");
				tryToRevertRelayState(relay, 1);
			}}, relay.getAutoRevert(), TimeUnit.SECONDS);
	}
	
	private void tryToRevertRelayState(Relay relay, int retry) {
		if (retry > 1) {
			if (retry > 3) {
				logger.error("Failed to revert relay state " + relay.getNameId());
				return;
			}
			else {
				logger.error("Retry (" + retry + ") to revert relay state " + relay.getNameId());
			}
		}
		try {
			controlService.setRelayState(relay, false);
		}
		catch (Exception e) {
			logger.error("Error when revert relay state " + relay.getNameId(), e);
			try {
				Thread.sleep(5000);
			} catch (InterruptedException ie) {
				logger.error("InterruptedException " + ie.getMessage());
			}
			tryToRevertRelayState(relay, retry + 1);
		}
	}
}
