package net.ionoff.center.server.control;

public class UnknownRelayDriverModelException extends RuntimeException {

	private static final long serialVersionUID = 1L;
	private String controlerIp; 
	
	public UnknownRelayDriverModelException(String controlerIp, String message) {
		super(message);
		this.controlerIp = controlerIp;
	} 
	
	public UnknownRelayDriverModelException(String message) {
		super(message);
	}

	public String getControlerIp() {
		return controlerIp;
	}
}
