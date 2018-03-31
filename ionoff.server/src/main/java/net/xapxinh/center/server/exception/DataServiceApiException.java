package net.xapxinh.center.server.exception;


public class DataServiceApiException extends DataServiceException {

	private static final long serialVersionUID = 1L;

	public DataServiceApiException() {
		super();
	}

	public DataServiceApiException(String playerId) {
		super(playerId);
	}
}
