package net.ionoff.center.server.entity;

public class ModeScene extends BaseObj {

	private static final long serialVersionUID = 1L;
	
	private Mode mode;
	private Zone zone;
	private Scene scene;
	
	public Mode getMode() {
		return mode;
	}
	public void setMode(Mode mode) {
		this.mode = mode;
	}
	
	public Zone getZone() {
		return zone;
	}
	public void setZone(Zone zone) {
		this.zone = zone;
	}
	
	public Scene getScene() {
		return scene;
	}
	public void setScene(Scene scene) {
		this.scene = scene;
	}
}
