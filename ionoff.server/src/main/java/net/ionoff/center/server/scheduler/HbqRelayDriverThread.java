package net.ionoff.center.server.scheduler;

import java.util.Arrays;
import java.util.List;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import net.ionoff.center.server.control.IControlService;
import net.ionoff.center.server.control.UnknownRelayDriverModelException;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.persistence.service.IRelayDriverService;
import net.ionoff.center.server.persistence.service.IRelayService;
import net.ionoff.center.server.relaydriver.api.RelayDriverConnectException;
import net.ionoff.center.server.relaydriver.api.RelayDriverException;
import net.ionoff.center.server.relaydriver.api.RelayDriverStatus;
import net.ionoff.center.shared.entity.RelayDriverModel;


@Component
@EnableAsync
@EnableScheduling
public class HbqRelayDriverThread {
	
	private static final Logger LOGGER = Logger.getLogger(HbqRelayDriverThread.class.getName());

	private static final long INTERVAl = 5000; // 5 seconds

	@Autowired
	IControlService controlService;
	
	@Autowired
	IRelayService relayService;
	
	@Autowired
	IRelayDriverService relayDriverService;

	@Scheduled(fixedDelay = INTERVAl)
    public void scanRelayDriversStatus() {
		// LOGGER.info("Interval update HBQ relay driver status...");
		List<RelayDriver> ec100RelayDrivers = relayDriverService.findByModel(RelayDriverModel.HBQ_EC100);
		for (RelayDriver relayDriver : ec100RelayDrivers) {
			scanRelayDriverStatus(relayDriver);
		}
		List<RelayDriver> ep2RelayDrivers = relayDriverService.findByModel(RelayDriverModel.HLAB_EP2);
		for (RelayDriver relayDriver : ep2RelayDrivers) {
			scanRelayDriverStatus(relayDriver);
		}
	}
	
	@Async
	protected void scanRelayDriverStatus(RelayDriver relayDriver) {
		boolean oldStatus = relayDriver.isConnected();
		try {
			pingContoller(relayDriver);
			relayDriver.setConnectedTime(System.currentTimeMillis());
			relayDriverService.update(relayDriver);
			final RelayDriverStatus relayDriverStatus = controlService.getRelayDriverStatus(relayDriver);
			updateRelayDriverStatus(relayDriver, true, oldStatus, relayDriverStatus);
		}
		catch (final Exception e) {
			LOGGER.error("Error "  + relayDriver.getIp() + ":" + relayDriver.getPort() + ": "+ e.getMessage());
			try {
				updateRelayDriverStatus(relayDriver, false, oldStatus, new RelayDriverStatus());
			} catch (RelayDriverConnectException ex) {
				LOGGER.error("RelayDriverConnectException: " + ex.getMessage());
			}
		}
	}

	private void pingContoller(RelayDriver relayDriver) throws RelayDriverConnectException {
		if (controlService.ping(relayDriver) == false) {
			throw new RelayDriverConnectException(relayDriver.getIp());
		}
	}

	private void updateRelayDriverStatus(RelayDriver relayDriver,
			boolean newStatus, boolean oldStatus, RelayDriverStatus relayDriverStatus) {

		if (newStatus == oldStatus) {
			//relayDriver status is not changed
			if (newStatus == true) {
				//LOGGER.info("RelayDriver " + relayDriver.getIp() + ":" + relayDriver.getPort() + " is connected");
				//relayDriver is still connected
				updateRelaysStatus(relayDriver, relayDriverStatus.getRelayOutputStatus());
				return;
			}
			//relayDriver is still disconnected
			//LOGGER.info("RelayDriver " + relayDriver.getIp() + ":" + relayDriver.getPort() + " is still disconnected");
			return;
		}
		
		if (newStatus == true) {
			LOGGER.info("RelayDriver " + relayDriver.getIp() + ":" + relayDriver.getPort() + " is now connected");
			//relayDriver status is changed from disconnected to connected
			onRelayDriverConnected(relayDriver, relayDriverStatus);
		}
		else {
			//relayDriver status is changed from connected to disconnected
			LOGGER.info("RelayDriver " + relayDriver.getIp() + " is now disconnected");
		}
	}

	private void onRelayDriverConnected(RelayDriver relayDriver, RelayDriverStatus relayDriverStatus) throws RelayDriverException, UnknownRelayDriverModelException {
		final List<Boolean> relayOutputsStatus = relayDriverStatus.getRelayOutputStatus();
		LOGGER.info("Restore relay status " + relayDriver.getIp() + ": " + Arrays.toString(relayOutputsStatus.toArray()));
		for (final Relay relay : relayDriver.getRelays()) {
			final Boolean newRelayStatus = relayOutputsStatus.get(relay.getIndex());
			if (relay.getStatus() != null && !relay.getStatus().equals(newRelayStatus)) {
				applyStoredRelayStatusToRelayDriver(relay);
			}
		}
	}

	private void applyStoredRelayStatusToRelayDriver(Relay relay) throws RelayDriverException, UnknownRelayDriverModelException {
		if (relay.getStatus() == false) {
			controlService.switchRelayToOff(relay);
		}
		else if (relay.getStatus() == true) {
			controlService.switchRelayToOn(relay);
		}
	}

	private void updateRelaysStatus(RelayDriver relayDriver, List<Boolean> relayStatus) {
		for (final Relay relay : relayDriver.getRelays()) {
			updateRelayStatus(relay, relayStatus.get(relay.getIndex()));
		}
	}

	private void updateRelayStatus(Relay relay, Boolean newStatus) {
		if (relay.updateStatus(newStatus)) {
			relayService.update(relay, newStatus);
		}
	}
}
