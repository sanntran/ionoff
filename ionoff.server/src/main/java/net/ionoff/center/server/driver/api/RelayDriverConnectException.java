package net.ionoff.center.server.driver.api;

public class RelayDriverConnectException extends RelayDriverException {

	private static final long serialVersionUID = 1L;
	
	public RelayDriverConnectException(String controlerIp) {
		super(controlerIp);
	}
}
