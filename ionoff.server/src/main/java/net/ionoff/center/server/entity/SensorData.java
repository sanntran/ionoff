package net.ionoff.center.server.entity;

import java.util.Date;

public class SensorData extends BaseObj {

	private static final long serialVersionUID = 1L;
	
	private Date time;
	private Double value;
	private Long index;
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

	public Long getIndex() {
		return index;
	}
	public void setIndex(Long index) {
		this.index = index;
	}

	public Sensor getSensor() {
		return sensor;
	}
	public void setSensor(Sensor sensor) {
		this.sensor = sensor;
	}
}
