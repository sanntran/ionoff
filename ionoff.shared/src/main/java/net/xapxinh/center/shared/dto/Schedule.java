package net.xapxinh.center.shared.dto;

public class Schedule extends SerializableDto {
	
	private static final long serialVersionUID = 1L;
	public static final String NONE = "none";
	public static final String STOP = "stop";
	public static final String SHUTDOWN = "shutdown";
	
	private String action;
	private String dateTime;
	
	public String getAction() {
		return action;
	}
	public void setAction(String action) {
		this.action = action;
	}
	public String getDateTime() {
		return dateTime;
	}
	public void setDateTime(String dateTime) {
		this.dateTime = dateTime;
	}
}
