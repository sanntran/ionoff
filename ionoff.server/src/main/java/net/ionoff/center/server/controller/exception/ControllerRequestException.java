package net.ionoff.center.server.controller.exception;

public class ControllerRequestException extends RuntimeException {

	private static final long serialVersionUID = 1L;
	
	public ControllerRequestException(String message) {
		super(message);
	}
}
