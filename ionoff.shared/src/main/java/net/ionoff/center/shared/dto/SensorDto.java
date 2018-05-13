package net.ionoff.center.shared.dto;

public class SensorDto extends BaseDto {

	private static final long serialVersionUID = 1L;
	
	private String unit;
	private String formula;
	private String type;
	private Long driverId;
	private String driverName;
	private Integer index;
	private Long deviceId;
	private String deviceName;
	private Long projectId;
	
	public String getType() {
		return type;
	}
	public void setType(String type) {
		this.type = type;
	}
	public Long getDriverId() {
		return driverId;
	}
	public void setDriverId(Long driverId) {
		this.driverId = driverId;
	}
	
	public String getDriverName() {
		return driverName;
	}
	public void setDriverName(String driverName) {
		this.driverName = driverName;
	}
	
	public Integer getIndex() {
		return index;
	}
	public void setIndex(Integer index) {
		this.index = index;
	}
	
	public Long getDeviceId() {
		return deviceId;
	}
	public void setDeviceId(Long deviceId) {
		this.deviceId = deviceId;
	}
	
	public String getDeviceName() {
		return deviceName;
	}
	public void setDeviceName(String deviceName) {
		this.deviceName = deviceName;
	}

	public Long getProjectId() {
		return projectId;
	}
	public void setProjectId(Long projectId) {
		this.projectId = projectId;
	}
	public String getUnit() {
		return unit;
	}
	public void setUnit(String unit) {
		this.unit = unit;
	}
	public String getFormula() {
		return formula;
	}
	public void setFormula(String formula) {
		this.formula = formula;
	}
}
