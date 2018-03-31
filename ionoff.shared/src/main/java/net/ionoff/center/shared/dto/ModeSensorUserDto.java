package net.ionoff.center.shared.dto;

public class ModeSensorUserDto extends BaseDto {

	private static final long serialVersionUID = 1L;
	
	private Long modeSensorId;
	private Boolean detected;
	private Long userId;
	private String userName;
	private Boolean sendSms;
	private Boolean sendEmail;
	
	public Boolean getDetected() {
		return detected;
	}
	public void setDetected(Boolean detected) {
		this.detected = detected;
	}
	
	public Long getModeSensorId() {
		return modeSensorId;
	}
	public void setModeSensorId(Long modeSensorId) {
		this.modeSensorId = modeSensorId;
	}
	
	public Long getUserId() {
		return userId;
	}
	public void setUserId(Long userId) {
		this.userId = userId;
	}
	
	public String getUserName() {
		return userName;
	}
	public void setUserName(String userName) {
		this.userName = userName;
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
	
	public boolean isSendSms() {
		return sendSms != null && sendSms.booleanValue() == true;
	}
	public boolean isSendEmail() {
		return sendEmail != null && sendEmail.booleanValue() == true;
	}
}
