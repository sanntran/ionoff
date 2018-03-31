package net.ionoff.center.server.entity;

public class ModeSensorUser extends BaseObj {

	private static final long serialVersionUID = 1L;
	
	private Boolean detected;
	private Boolean sendSms;
	private Boolean sendEmail;
	private ModeSensor modeSensor;
	private User user;
	private Project project;
	
	public Boolean getDetected() {
		return detected;
	}
	public void setDetected(Boolean detected) {
		this.detected = detected;
	}

	public ModeSensor getModeSensor() {
		return modeSensor;
	}
	public void setModeSensor(ModeSensor modeSensor) {
		this.modeSensor = modeSensor;
	}
	
	public Boolean getSendSms() {
		return sendSms;
	}
	public void setSendSms(Boolean sendSms) {
		this.sendSms = sendSms;
	}
	
	public Boolean getSendEmail() {
		return sendEmail;
	}
	public void setSendEmail(Boolean sendEmail) {
		this.sendEmail = sendEmail;
	}
	
	public User getUser() {
		return user;
	}
	public void setUser(User user) {
		this.user = user;
	}
	
	public boolean isSendEmail() {
		return sendEmail != null && sendEmail.booleanValue() == true;
	}
	public boolean isSendSms() {
		return sendSms != null && sendSms.booleanValue() == true;
	}
	
	public Project getProject() {
		return project;
	}
	public void setProject(Project project) {
		this.project = project;
	}
}
