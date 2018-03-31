package net.ionoff.center.server.entity;

import java.util.List;

public class SceneDevice extends BaseObj {

	private static final long serialVersionUID = 1L;
	
	private Integer order;
	private Integer duration;	
	private Scene scene;
	private Device device;
	private List<SceneAction> actions;
	
	public Integer getOrder() {
		return order;
	}
	public void setOrder(Integer order) {
		this.order = order;
	}
	
	public Integer getDuration() {
		return duration;
	}
	public void setDuration(Integer duration) {
		this.duration = duration;
	}
	
	public Scene getScene() {
		return scene;
	}
	public void setScene(Scene scene) {
		this.scene = scene;
	}
	
	public Device getDevice() {
		return device;
	}
	public void setDevice(Device device) {
		this.device = device;
	}
	
	public List<SceneAction> getActions() {
		return actions;
	}
	public void setActions(List<SceneAction> actions) {
		this.actions = actions;
	}
}
