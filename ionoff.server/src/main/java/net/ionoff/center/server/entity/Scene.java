package net.ionoff.center.server.entity;

import java.util.Date;
import java.util.List;

public class Scene extends BaseObj implements ISlice {

	private static final long serialVersionUID = 1L;
	
	private Date time;

	private Integer order;
	private Zone zone;
	private List<SceneDevice> devices;
	
	public Date getTime() {
		return time;
	}
	public void setTime(Date time) {
		this.time = time;
	}

	public Integer getOrder() {
		return order;
	}
	public void setOrder(Integer order) {
		this.order = order;
	}

	public Zone getZone() {
		return zone;
	}
	public void setZone(Zone zone) {
		this.zone = zone;
	}
	
	public List<SceneDevice> getDevices() {
		return devices;
	}
	public void setDevices(List<SceneDevice> sceneDevices) {
		this.devices = sceneDevices;
	}
}
