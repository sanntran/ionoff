package net.ionoff.center.server.entity;

import java.util.Date;

public class SensorData extends BaseObj {

	private static final long serialVersionUID = 1L;
	
	private Date time;
	private Double value;
	private Double total;
	private Sensor sensor;
	
	public Date getTime() {
		return time;
	}
	public void setTime(Date time) {
		this.time = time;
	}
	
	public Double getValue() {
		return value;
	}
	public void setValue(Double value) {
		this.value = value;
	}
	
	public Double getTotal() {
		return total;
	}
	public void setTotal(Double total) {
		this.total = total;
	}
	public Sensor getSensor() {
		return sensor;
	}
	public void setSensor(Sensor sensor) {
		this.sensor = sensor;
	}
}
