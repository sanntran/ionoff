package net.ionoff.center.server.relaydriver.api;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.control.UnknownRelayDriverModelException;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.shared.entity.RelayDriverModel;

public class RelayDriverApiProvider {
	
	@Autowired
	private E4RelayDriverApi e4RelayDriverApi;
	@Autowired
	private P4RelayDriverApi p4RelayDriverApi;
	@Autowired
	private P8RelayDriverApi p8RelayDriverApi;
	@Autowired
	private EC100RelayDriverApi ec100RelayDriverApi;
	@Autowired
	private EP2RelayDriverApi ep2Api;
	
	public IRelayDriverApi getRelayDriverApi(RelayDriver relayDriver) throws UnknownRelayDriverModelException {
		if (RelayDriverModel.IONOFF_E4.toString().equals(relayDriver.getModel())) {
			return e4RelayDriverApi;
		}
		if (RelayDriverModel.IONOFF_P4.toString().equals(relayDriver.getModel())) {
			return p4RelayDriverApi;
		}
		if (RelayDriverModel.IONOFF_P8.toString().equals(relayDriver.getModel())) {
			return p8RelayDriverApi;
		}
		if (RelayDriverModel.HBQ_EC100.toString().equals(relayDriver.getModel())) {
			return ec100RelayDriverApi;
		}
		if (RelayDriverModel.HLAB_EP2.toString().equals(relayDriver.getModel())) {
			return ep2Api;
		}
		throw new UnknownRelayDriverModelException(relayDriver.getModel());
	}
}
