package net.ionoff.center.server.controller.api;

public class ControllerConnectException extends ControllerException {

	private static final long serialVersionUID = 1L;
	
	public ControllerConnectException(String controlerIp) {
		super(controlerIp);
	}
}
