package net.ionoff.webhook.dto;

import org.springframework.http.HttpStatus;

import java.io.Serializable;

public class MessageDto implements Serializable {
	
	private static final long serialVersionUID = 1L;


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

	public MessageDto(int status, String code, String message) {
		this.status = status;
		this.code = code;
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
	
	public static MessageDto error(String message) {return new MessageDto(HttpStatus.INTERNAL_SERVER_ERROR.value(),
			HttpStatus.INTERNAL_SERVER_ERROR.getReasonPhrase(), message);
	}
	
	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		this.code = code;
	}

	public static MessageDto success(String message) {
		return new MessageDto(HttpStatus.OK.value(), HttpStatus.OK.getReasonPhrase(), message);
	}

}
