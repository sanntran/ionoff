package net.ionoff.center.shared.dto;

public class SensorDto extends BaseDto {

	private static final long serialVersionUID = 1L;
	public static final int NULL_INPUT = -1;
	
	private Long controllerId;
	private String controllerName;
	private Integer controllerInput;
	
	private Long projectId;
	
	public Long getControllerId() {
		return controllerId;
	}
	public void setControllerId(Long controllerId) {
		this.controllerId = controllerId;
	}
	
	public String getControllerName() {
		return controllerName;
	}
	public void setControllerName(String controllerName) {
		this.controllerName = controllerName;
	}
	
	public Integer getControllerInput() {
		return controllerInput;
	}
	public void setControllerInput(Integer controllerInput) {
		this.controllerInput = controllerInput;
	}
	public Long getProjectId() {
		return projectId;
	}
	public void setProjectId(Long projectId) {
		this.projectId = projectId;
	}
}
