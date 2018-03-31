package net.ionoff.center.server.controller.api;

public class ControllerException extends RuntimeException {

	private static final long serialVersionUID = 1L;
	
	public ControllerException(String controlerIp) {
		super(controlerIp);
	}
}
