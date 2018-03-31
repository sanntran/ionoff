package net.ionoff.center.server.controller.api;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.control.UnknownControllerModelException;
import net.ionoff.center.server.entity.Controller;
import net.ionoff.center.shared.entity.ControllerModel;

public class ControllerApiProvider {
	
	@Autowired
	private E4ControllerApi e4ControllerApi;
	@Autowired
	private P4ControllerApi p4ControllerApi;
	@Autowired
	private P8ControllerApi p8ControllerApi;
	@Autowired
	private EC100ControllerApi ec100ControllerApi;
	@Autowired
	private EP2ControllerApi ep2Api;
	
	public IControllerApi getControllerApi(Controller controller) throws UnknownControllerModelException {
		if (ControllerModel.IONOFF_E4.toString().equals(controller.getModel())) {
			return e4ControllerApi;
		}
		if (ControllerModel.IONOFF_P4.toString().equals(controller.getModel())) {
			return p4ControllerApi;
		}
		if (ControllerModel.IONOFF_P8.toString().equals(controller.getModel())) {
			return p8ControllerApi;
		}
		if (ControllerModel.HBQ_EC100.toString().equals(controller.getModel())) {
			return ec100ControllerApi;
		}
		if (ControllerModel.HLAB_EP2.toString().equals(controller.getModel())) {
			return ep2Api;
		}
		throw new UnknownControllerModelException(controller.getModel());
	}
}
