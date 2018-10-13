package net.ionoff.center.shared.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;

public class SensorDriverDto extends DeviceDto {

	private static final long serialVersionUID = 1L;
	
	private String mac;
	private String model;
	
	@Override
	public String getClazz() {
		return "SensorDriverDto";
	}
	
	public String getMac() {
		return mac;
	}

	public void setMac(String mac) {
		this.mac = mac;
	}
	
	public Boolean isConnected() {
		return false;
	}

	public String getModel() {
		return model;
	}

	public void setModel(String model) {
		this.model = model;
	}

}
