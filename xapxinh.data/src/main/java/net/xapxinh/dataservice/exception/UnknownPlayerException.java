package net.xapxinh.dataservice.exception;

public class UnknownPlayerException extends Exception {

	private static final long serialVersionUID = 1L;

	public UnknownPlayerException(String mac) {
		super("Unknown player: " + mac);
	}
}
