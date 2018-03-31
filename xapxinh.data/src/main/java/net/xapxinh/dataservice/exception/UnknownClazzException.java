package net.xapxinh.dataservice.exception;

public class UnknownClazzException extends Exception {

	private static final long serialVersionUID = 1L;

	public UnknownClazzException(String clazz) {
		super("Unknown clazz: " + clazz);
	}
}
