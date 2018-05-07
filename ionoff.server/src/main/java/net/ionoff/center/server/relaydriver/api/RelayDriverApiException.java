package net.ionoff.center.server.relaydriver.api;

public class RelayDriverApiException extends RelayDriverException {

	private static final long serialVersionUID = 1L;
	
	public RelayDriverApiException(String controlerIp) {
		super(controlerIp);
	}
}
