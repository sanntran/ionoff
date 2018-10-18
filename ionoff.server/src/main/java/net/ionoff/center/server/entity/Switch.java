package net.ionoff.center.server.entity;

import java.util.Date;
import java.util.List;

public class Switch extends BaseObj {

	private static final long serialVersionUID = 1L;
	public static final int NULL_INPUT = -1;
	
	private Integer index;
	private Date time;
	private Boolean status;
	private Controller driver;
	private List<Sensor> sensors;
	
	public Integer getIndex() {
		return index;
	}
	public void setIndex(Integer index) {
		this.index = index;
	}

	public Date getTime() {
		return time;
	}
	public void setTime(Date time) {
		this.time = time;
	}
	
	public Boolean getStatus() {
		return status;
	}
	public void setStatus(Boolean status) {
		this.status = status;
	}
	
	public Controller getDriver() {
		return driver;
	}
	public void setDriver(Controller driver) {
		this.driver = driver;
	}
	
	public boolean updateStatus(Boolean newStatus) {
		if (newStatus == null) {
			return false;
		}
		if (status != null && status.equals(newStatus)) {
			return false;
		}
		status = newStatus;
		time = new Date();
		return true;
	}
	
	public List<Sensor> getSensors() {
		return sensors;
	}
	
	public void setSensors(List<Sensor> sensors) {
		this.sensors = sensors;
	}
	
}
