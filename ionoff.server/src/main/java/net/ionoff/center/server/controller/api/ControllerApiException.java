package net.ionoff.center.server.controller.api;

public class ControllerApiException extends ControllerException {

	private static final long serialVersionUID = 1L;
	
	public ControllerApiException(String controlerIp) {
		super(controlerIp);
	}
}
