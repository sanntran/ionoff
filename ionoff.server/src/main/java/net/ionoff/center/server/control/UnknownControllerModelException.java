package net.ionoff.center.server.control;

public class UnknownControllerModelException extends RuntimeException {

	private static final long serialVersionUID = 1L;
	private String controlerIp;
	
	public UnknownControllerModelException(String controlerIp, String message) {
		super(message);
		this.controlerIp = controlerIp;
	}
	
	public UnknownControllerModelException(String message) {
		super(message);
	}

	public String getControlerIp() {
		return controlerIp;
	}
}
