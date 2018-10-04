package net.ionoff.center.server.relaydriver.exception;

public class RelayDriverException extends RuntimeException {

	private static final long serialVersionUID = 1L;
	
	public RelayDriverException(String controlerIp) {
		super(controlerIp);
	}
}
