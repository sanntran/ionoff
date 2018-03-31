package net.ionoff.center.server.entity;

public class Sensor extends BaseObj {

	private static final long serialVersionUID = 1L;
	public static final int NULL_INPUT = -1;
	private Boolean status;
	private Project project;
	private Controller controller;
	private Integer controllerInput;
	
	public Boolean getStatus() {
		return status;
	}
	public void setStatus(Boolean status) {
		this.status = status;
	}
	
	public Project getProject() {
		return project;
	}	
	public void setProject(Project project) {
		this.project = project;
	}
	
	public Controller getController() {
		return controller;
	}
	public void setController(Controller controller) {
		this.controller = controller;
	}
	
	public Integer getControllerInput() {
		return controllerInput;
	}
	public void setControllerInput(Integer controllerInput) {
		this.controllerInput = controllerInput;
	}
}
