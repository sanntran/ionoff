package net.xapxinh.dataservice.entity;

import java.io.Serializable;

public class Player implements Serializable {

	private static final long serialVersionUID = 1L;
	
	private Long id;
	private String mac;
	
	private String licenseKey;	
	private Long connectedTime;
	private Integer acceptRequest;
	
	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}
	
	public String getMac() {
		return mac;
	}

	public void setMac(String mac) {
		this.mac = mac;
	}

	public String getLicenseKey() {
		return licenseKey;
	}

	public void setLicenseKey(String licenseKey) {
		this.licenseKey = licenseKey;
	}

	public Long getConnectedTime() {
		return connectedTime;
	}

	public void setConnectedTime(Long connectedTime) {
		this.connectedTime = connectedTime;
	}

	public Integer getAcceptRequest() {
		return acceptRequest;
	}

	public void setAcceptRequest(Integer acceptRequest) {
		this.acceptRequest = acceptRequest;
	}
}
