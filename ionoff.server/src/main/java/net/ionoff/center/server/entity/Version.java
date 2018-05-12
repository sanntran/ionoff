package net.ionoff.center.server.entity;

public class Version extends BaseObj {

	private static final long serialVersionUID = 1L;
	
	private String dateTime;	
	private String fileName;
	
	public String getDateTime() {
		return dateTime;
	}

	public void setDateTime(String dateTime) {
		this.dateTime = dateTime;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}
	
}
