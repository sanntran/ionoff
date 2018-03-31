package net.ionoff.center.server.thread;

import net.ionoff.center.server.control.IControlService;
import net.ionoff.center.server.control.UnknownControllerModelException;
import net.ionoff.center.server.controller.api.ControllerException;
import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.server.persistence.service.IControllerService;
import net.ionoff.center.server.persistence.service.IRelayService;

class Ep2ControllerThread extends AbstractControllerThread {

	Ep2ControllerThread(Long controllerId,
			IControllerService controllerService,
			IControlService controlService,
			IRelayService relayService
			) {
		super(controllerId, controllerService, controlService, relayService);
	}
	
	@Override
	protected void scanControllerStatus() throws ControllerException, UnknownControllerModelException, ControllerNotFoundException {
		final Controller controller = controllerService.findById(controllerId);
		if (controller == null) {
			throw new ControllerNotFoundException();
		}
		scanControllerStatus(controller);
	}
}
