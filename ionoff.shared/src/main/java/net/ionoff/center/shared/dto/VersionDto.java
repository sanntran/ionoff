package net.ionoff.center.shared.dto;

public class VersionDto extends BaseDto {

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
