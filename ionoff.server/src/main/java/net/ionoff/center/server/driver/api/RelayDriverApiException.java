package net.ionoff.center.server.driver.api;

public class RelayDriverApiException extends RelayDriverException {

	private static final long serialVersionUID = 1L;
	
	public RelayDriverApiException(String controlerIp) {
		super(controlerIp);
	}
}
