package net.ionoff.center.server.thread;

import net.ionoff.center.server.control.IControlService;
import net.ionoff.center.server.control.UnknownRelayDriverModelException;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.server.persistence.service.IRelayDriverService;
import net.ionoff.center.server.persistence.service.IRelayService;
import net.ionoff.center.server.relaydriver.api.RelayDriverException;

class Ec100RelayDriverThread extends AbstractRelayDriverThread {

	Ec100RelayDriverThread(Long relayDriverId,
			IRelayDriverService relayDriverService, 
			IControlService controlService,
			IRelayService relayService
			) {
		super(relayDriverId, relayDriverService, controlService, relayService);
	}
	
	@Override
	protected void scanRelayDriverStatus() throws RelayDriverException, UnknownRelayDriverModelException, RelayDriverNotFoundException {
		final RelayDriver relayDriver = relayDriverService.findById(relayDriverId);
		if (relayDriver == null) {
			throw new RelayDriverNotFoundException();
		}
		scanRelayDriverStatus(relayDriver);
	}
}
