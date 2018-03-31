package net.xapxinh.center.server.api.data;

import net.xapxinh.center.server.exception.DataServiceException;

public class UnknownPlayerException extends DataServiceException {

	private static final long serialVersionUID = 1L;

	public UnknownPlayerException(String message) {
		super(message);
	}
}
