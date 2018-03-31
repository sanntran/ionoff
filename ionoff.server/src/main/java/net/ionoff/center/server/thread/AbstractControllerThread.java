package net.ionoff.center.server.thread;

import java.util.Arrays;
import java.util.List;

import org.apache.log4j.Logger;

import net.ionoff.center.server.control.IControlService;
import net.ionoff.center.server.control.UnknownControllerModelException;
import net.ionoff.center.server.controller.api.ControllerConnectException;
import net.ionoff.center.server.controller.api.ControllerException;
import net.ionoff.center.server.controller.api.ControllerStatus;
import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.entity.Relay;
import net.ionoff.center.server.persistence.service.IControllerService;
import net.ionoff.center.server.persistence.service.IRelayService;

public abstract class AbstractControllerThread extends Thread {
	private static final Logger LOGGER = Logger.getLogger(AbstractControllerThread.class.getName());

	private static final long INTERVAl = 5000; // 5 seconds

	protected Long controllerId;
	protected final String threadName;
	protected final IControlService controlService;
	protected final IRelayService relayService;
	protected final IControllerService controllerService;
	
	public AbstractControllerThread(Long controllerId,
			IControllerService controllerService,
			IControlService controlService,
			IRelayService relayService
			) {
		threadName = "[Controller #" + controllerId + "]";
		this.controllerId = controllerId;
		this.controlService = controlService;
		this.relayService = relayService;
		this.controllerService = controllerService;
	}

	@Override
	public void run() {
		LOGGER.info(threadName + " Thread has been started !");
		for (; true;) {
			try {
				sleep(INTERVAl);
				if (controllerId == null) {
					throw new ControllerNotFoundException();
				}
				scanControllerStatus();
			}
			catch (final Throwable e) {
				if (e instanceof ControllerNotFoundException) {
					LOGGER.error(threadName + " Controller is null. Thread " + threadName + " is destroyed now!");
					return;
				}
				LOGGER.error(e.getMessage(), e);
				if (e instanceof OutOfMemoryError) {
					System.gc();
				}
			}
		}
	}
	
	protected void scanControllerStatus() throws ControllerNotFoundException, ControllerException, UnknownControllerModelException {
		final Controller controller = controllerService.findById(controllerId);
		if (controller == null) {
			throw new ControllerNotFoundException();
		}
		scanControllerStatus(controller);
	}
	
	protected void scanControllerStatus(Controller controller) throws ControllerException, UnknownControllerModelException {
		boolean oldStatus = controller.isConnected();
		try {
			//LOGGER.info(threadName + " Send request status to " + controller.getIp());
			pingContoller(controller);
			controller.setConnectedTime(System.currentTimeMillis());
			controllerService.update(controller);
			final ControllerStatus controllerStatus = controlService.getControllerStatus(controller);
			updateControllerStatus(controller, true, oldStatus, controllerStatus);
		}
		catch (final ControllerConnectException e) {
			try {
				updateControllerStatus(controller, false, oldStatus, new ControllerStatus());
			} catch (ControllerConnectException ex) {
				LOGGER.error(e.getMessage(), ex);
			}
		}
	}

	private void pingContoller(Controller controller) throws ControllerConnectException {
		if (controlService.ping(controller) == false) {
			throw new ControllerConnectException(controller.getIp());
		}
	}

	private void updateControllerStatus(Controller controller,
			boolean newStatus, boolean oldStatus, ControllerStatus controllerStatus) throws ControllerException, UnknownControllerModelException {

		if (newStatus == oldStatus) {
			//controller status is not changed
			if (newStatus == true) {
				//LOGGER.info("Controller " + controller.getIp() + ":" + controller.getPort() + " is connected");
				//controller is still connected
				updateRelaysStatus(controller, controllerStatus.getRelayOutputStatus());
				return;
			}
			//controller is still disconnected
			//LOGGER.info("Controller " + controller.getIp() + ":" + controller.getPort() + " is still disconnected");
			return;
		}
		
		if (newStatus == true) {
			LOGGER.info("Controller " + controller.getIp() + ":" + controller.getPort() + " is now connected");
			//controller status is changed from disconnected to connected
			onControllerConnected(controller, controllerStatus);
		}
		else {
			//controller status is changed from connected to disconnected
			LOGGER.info("Controller " + controller.getIp() + " is now disconnected");
		}
	}

	private void onControllerConnected(Controller controller, ControllerStatus controllerStatus) throws ControllerException, UnknownControllerModelException {
		final List<Boolean> relayOutputsStatus = controllerStatus.getRelayOutputStatus();
		LOGGER.info("Restore relay status " + controller.getIp() + ": " + Arrays.toString(relayOutputsStatus.toArray()));
		for (final Relay relay : controller.getRelays()) {
			final Boolean newRelayStatus = relayOutputsStatus.get(relay.getIndex());
			if (relay.getStatus() != null && !relay.getStatus().equals(newRelayStatus)) {
				applyStoredRelayStatusToController(relay);
			}
		}
	}

	private void applyStoredRelayStatusToController(Relay relay) throws ControllerException, UnknownControllerModelException {
		if (relay.getStatus() == false) {
			controlService.setRelayOff(relay);
		}
		else if (relay.getStatus() == true) {
			controlService.setRelayOn(relay);
		}
	}

	private void updateRelaysStatus(Controller controller, List<Boolean> relayStatus) {
		for (final Relay relay : controller.getRelays()) {
			updateRelayStatus(relay, relayStatus.get(relay.getIndex()));
		}
	}

	private void updateRelayStatus(Relay relay, Boolean newStatus) {
		if (relay.updateStatus(newStatus)) {
			relayService.update(relay, newStatus);
		}
	}

	public void terminate() {
		controllerId = null;
		interrupt();
	}
}
