package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class ModeSensorUser implements IEntity {

	private static final long serialVersionUID = 1L;

	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private Boolean sendSms;
	private Boolean sendEmail;
	private ModeSensor modeSensor;
	private User user;
	private Project project;
	
	public boolean izSendEmail() {
		return sendEmail != null && sendEmail;
	}
	public boolean izSendSms() {
		return sendSms != null && sendSms;
	}

}
