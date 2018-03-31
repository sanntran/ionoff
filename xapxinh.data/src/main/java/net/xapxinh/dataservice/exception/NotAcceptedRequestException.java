package net.xapxinh.dataservice.exception;

public class NotAcceptedRequestException extends Exception {

	private static final long serialVersionUID = 1L;

	public NotAcceptedRequestException(String playerName) {
		super("Request from " + playerName + " is not accepted");
	}
}
