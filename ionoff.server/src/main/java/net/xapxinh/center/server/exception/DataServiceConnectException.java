package net.xapxinh.center.server.exception;


public class DataServiceConnectException extends DataServiceException {

	private static final long serialVersionUID = 1L;

	public DataServiceConnectException() {
		super();
	}

	public DataServiceConnectException(String playerId) {
		super(playerId);
	}
}
