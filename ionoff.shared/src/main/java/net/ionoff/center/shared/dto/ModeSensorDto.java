package net.ionoff.center.shared.dto;

public class ModeSensorDto extends BaseDto {

	private static final long serialVersionUID = 1L;
	
	private Boolean enabled;
	private Boolean detected;
	private Integer timeBuffer;
	private Long modeId;
	private String modeName;
	private Long sensorId;
	private String sensorName;
	
	public Boolean getEnabled() {
		return enabled != null ? enabled : false;
	}
	public void setEnabled(Boolean enabled) {
		this.enabled = enabled;
	}
	
	public Boolean getDetected() {
		return detected;
	}
	public void setDetected(Boolean detected) {
		this.detected = detected;
	}
	
	public Integer getTimeBuffer() {
		return timeBuffer;
	}
	public void setTimeBuffer(Integer timeBuffer) {
		this.timeBuffer = timeBuffer;
	}
	
	public Long getModeId() {
		return modeId;
	}
	public void setModeId(Long modeId) {
		this.modeId = modeId;
	}
	
	public String getModeName() {
		return modeName;
	}
	public void setModeName(String modeName) {
		this.modeName = modeName;
	}
	
	public Long getSensorId() {
		return sensorId;
	}
	public void setSensorId(Long sensorId) {
		this.sensorId = sensorId;
	}
	
	public String getSensorName() {
		return sensorName;
	}
	public void setSensorName(String sensorName) {
		this.sensorName = sensorName;
	}
}
