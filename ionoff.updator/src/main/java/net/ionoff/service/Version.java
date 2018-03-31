package net.ionoff.service;

import java.io.Serializable;

public class Version implements Serializable {

	private static final long serialVersionUID = 1L;
	
	private String name;
	private String dateTime;	
	private String fileName;	

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

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
