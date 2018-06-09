package net.ionoff.center.server.entity;

public class Sensor extends BaseObj {

	private static final long serialVersionUID = 1L;
	
	public static enum TYPE {
		ANALOG, DIGITAL;
	}
	private Integer order;
	private String unit;
	private String type;
	private Zone zone;
	private Device device;
	private Project project;
	private Switch zwitch;
	private SensorStatus status;

	public Integer getOrder() {
		return order;
	}

	public void setOrder(Integer order) {
		this.order = order;
	}

	public String getUnit() {
		return unit;
	}
	public void setUnit(String unit) {
		this.unit = unit;
	}
	
	public String getType() {
		return type;
	}
	public void setType(String type) {
		this.type = type;
	}
	
	public SensorStatus getStatus() {
		return status;
	}
	public void setStatus(SensorStatus status) {
		this.status = status;
	}
	
	public Zone getZone() {
		return zone;
	}
	public void setZone(Zone zone) {
		this.zone = zone;
	}
	
	public Device getDevice() {
		return device;
	}
	public void setDevice(Device device) {
		this.device = device;
	}
	public Project getProject() {
		return project;
	}	
	public void setProject(Project project) {
		this.project = project;
	}
	
	public Switch getZwitch() {
		return zwitch;
	}
	public void setZwitch(Switch zwitch) {
		this.zwitch = zwitch;
	}
}
