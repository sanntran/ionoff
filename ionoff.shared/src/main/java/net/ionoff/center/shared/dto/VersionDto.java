package net.ionoff.center.shared.dto;

public class VersionDto extends BaseDto {

	private static final long serialVersionUID = 1L;
	
	private String dateTime;

	public String getDateTime() {
		return dateTime;
	}

	public void setDateTime(String dateTime) {
		this.dateTime = dateTime;
	}
}
