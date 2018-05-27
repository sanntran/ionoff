package net.ionoff.center.server.entity;

import java.util.List;

public class SensorDriver extends Device {

	private static final long serialVersionUID = 1L;

	private String mac;
	private String model;
	private List<Sensor> sensors;
	
	public String getMac() {
		return mac;
	}
	public void setMac(String mac) {
		this.mac = mac;
	}
	
	public String getModel() {
		return model;
	}
	public void setModel(String model) {
		this.model = model;
	}
	
	public List<Sensor> getSensors() {
		return sensors;
	}
	public void setSensors(List<Sensor> sensors) {
		this.sensors = sensors;
	}
	

	@Override
	public Boolean getStatus() {
		if (getTime() == null || (System.currentTimeMillis() - getTime().getTime()) > 60000) {
			return false;
		}
		return true;
	}
	
	public boolean isOnline() {
		return getStatus().booleanValue();
	}

    public boolean hasSensor() {
		return sensors != null && !sensors.isEmpty();
    }
}
