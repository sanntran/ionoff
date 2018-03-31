package net.xapxinh.center.server.api.data;

import net.xapxinh.center.server.exception.DataServiceException;

public class DataServiceConnectException extends DataServiceException {

	private static final long serialVersionUID = 1L;

	public DataServiceConnectException() {
		super();
	}

	public DataServiceConnectException(String message) {
		super(message);
	}
}
