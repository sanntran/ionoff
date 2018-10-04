package net.ionoff.center.server.driver.api;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.control.UnknownRelayDriverModelException;
import net.ionoff.center.server.entity.RelayDriver;
import net.ionoff.center.shared.entity.RelayDriverModel;

public class RelayDriverProvider {




	@Autowired
	private E3RelayDriver e3RelayDriverApi;
	@Autowired
	private E4RelayDriver e4RelayDriverApi;
	@Autowired
	private P4RelayDriver p4RelayDriverApi;
	@Autowired
	private P8RelayDriver p8RelayDriverApi;
	@Autowired
	private EC100RelayDriver ec100RelayDriverApi;
	@Autowired
	private EP2RelayDriver ep2Api;
	
	public IRelayDriver getRelayDriverApi(RelayDriver relayDriver) throws UnknownRelayDriverModelException {
		if (RelayDriverModel.IONOFF_E3.toString().equals(relayDriver.getModel())) {
			return e3RelayDriverApi;
		}
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
