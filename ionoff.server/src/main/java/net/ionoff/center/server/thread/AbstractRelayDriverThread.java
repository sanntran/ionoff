package net.ionoff.center.server.thread;

import java.util.Arrays;
import java.util.List;

import org.apache.log4j.Logger;

import net.ionoff.center.server.control.IControlService;
import net.ionoff.center.server.control.UnknownRelayDriverModelException;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.persistence.service.IRelayDriverService;
import net.ionoff.center.server.persistence.service.IRelayService;
import net.ionoff.center.server.relaydriver.api.RelayDriverConnectException;
import net.ionoff.center.server.relaydriver.api.RelayDriverException;
import net.ionoff.center.server.relaydriver.api.RelayDriverStatus;

public abstract class AbstractRelayDriverThread extends Thread {
	private static final Logger LOGGER = Logger.getLogger(AbstractRelayDriverThread.class.getName());

	private static final long INTERVAl = 5000; // 5 seconds

	protected Long relayDriverId;
	protected final String threadName;
	protected final IControlService controlService;
	protected final IRelayService relayService;
	protected final IRelayDriverService relayDriverService;
	
	public AbstractRelayDriverThread(Long relayDriverId,
			IRelayDriverService relayDriverService,
			IControlService controlService,
			IRelayService relayService
			) {
		threadName = "[RelayDriver #" + relayDriverId + "]";
		this.relayDriverId = relayDriverId;
		this.controlService = controlService;
		this.relayService = relayService;
		this.relayDriverService = relayDriverService;
	}

	@Override
	public void run() {
		LOGGER.info(threadName + " Thread has been started !");
		for (; true;) {
			try {
				sleep(INTERVAl);
				if (relayDriverId == null) {
					throw new RelayDriverNotFoundException();
				}
				scanRelayDriverStatus();
			}
			catch (final Throwable e) {
				if (e instanceof RelayDriverNotFoundException) {
					LOGGER.error(threadName + " RelayDriver is null. Thread " + threadName + " is destroyed now!");
					return;
				}
				LOGGER.error(e.getMessage(), e);
				if (e instanceof OutOfMemoryError) {
					System.gc();
				}
			}
		}
	}
	
	protected void scanRelayDriverStatus() throws RelayDriverNotFoundException, RelayDriverException, UnknownRelayDriverModelException {
		final RelayDriver relayDriver = relayDriverService.findById(relayDriverId);
		if (relayDriver == null) {
			throw new RelayDriverNotFoundException();
		}
		scanRelayDriverStatus(relayDriver);
	}
	
	protected void scanRelayDriverStatus(RelayDriver relayDriver) throws RelayDriverException, UnknownRelayDriverModelException {
		boolean oldStatus = relayDriver.isConnected();
		try {
			//LOGGER.info(threadName + " Send request status to " + relayDriver.getIp());
			pingContoller(relayDriver);
			relayDriver.setConnectedTime(System.currentTimeMillis());
			relayDriverService.update(relayDriver);
			final RelayDriverStatus relayDriverStatus = controlService.getRelayDriverStatus(relayDriver);
			updateRelayDriverStatus(relayDriver, true, oldStatus, relayDriverStatus);
		}
		catch (final RelayDriverConnectException e) {
			try {
				updateRelayDriverStatus(relayDriver, false, oldStatus, new RelayDriverStatus());
			} catch (RelayDriverConnectException ex) {
				LOGGER.error(e.getMessage(), ex);
			}
		}
	}

	private void pingContoller(RelayDriver relayDriver) throws RelayDriverConnectException {
		if (controlService.ping(relayDriver) == false) {
			throw new RelayDriverConnectException(relayDriver.getIp());
		}
	}

	private void updateRelayDriverStatus(RelayDriver relayDriver,
			boolean newStatus, boolean oldStatus, RelayDriverStatus relayDriverStatus) throws RelayDriverException, UnknownRelayDriverModelException {

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
			controlService.setRelayOff(relay);
		}
		else if (relay.getStatus() == true) {
			controlService.setRelayOn(relay);
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

	public void terminate() {
		relayDriverId = null;
		interrupt();
	}
}
