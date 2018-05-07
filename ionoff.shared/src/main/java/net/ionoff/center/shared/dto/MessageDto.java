package net.ionoff.center.shared.dto;

import java.io.Serializable;

public class MessageDto implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	public static final int SC_200 = 200;
	public static final int SC_400 = 400;
	public static final String RelayDriverConnectException = "RelayDriverConnectException";
	

	private int status;
	private String code;
	private String message;
	
	public MessageDto() {
		//
	}
	
	public MessageDto(int status, String message) {
		this.status = status;
		this.message = message;
	}

	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.code = String.valueOf(status);
		this.status = status;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}
	
	public static MessageDto error(String message) {
		return new MessageDto(SC_400, message);
	}
	
	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		this.code = code;
	}

	public static MessageDto success(String message) {
		return new MessageDto(SC_200, message);
	}
	
	public static MessageDto success(long message) {
		return new MessageDto(SC_200, String.valueOf(message));
	}
	
	public static MessageDto success(boolean message) {
		return new MessageDto(SC_200, String.valueOf(message));
	}
}
