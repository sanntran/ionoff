package net.ionoff.center.server.entity;

public class ModeSensorScene extends BaseObj {

	private static final long serialVersionUID = 1L;
	
	private Boolean detected;
	private ModeSensor modeSensor;
	private Zone zone;
	private Scene scene;
	
	public Boolean getDetected() {
		return detected;
	}
	public void setDetected(Boolean detected) {
		this.detected = detected;
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
	
	public ModeSensor getModeSensor() {
		return modeSensor;
	}
	public void setModeSensor(ModeSensor modeSensor) {
		this.modeSensor = modeSensor;
	}
}
