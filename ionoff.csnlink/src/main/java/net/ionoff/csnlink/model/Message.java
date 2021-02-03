package net.ionoff.csnlink.model;

import org.springframework.http.HttpStatus;

import java.io.Serializable;


public class Message implements Serializable {
	private static final long serialVersionUID = 1L;


	private int status;
	private String code;
	private String message;
	
	public Message() {
		//
	}
	
	public Message(int status, String message) {
		this.status = status;
		this.message = message;
	}

	public Message(int status, String code, String message) {
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
	
	public static Message error(String message) {return new Message(HttpStatus.INTERNAL_SERVER_ERROR.value(),
			HttpStatus.INTERNAL_SERVER_ERROR.getReasonPhrase(), message);
	}
	
	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		this.code = code;
	}

	public static Message success(String message) {
		return new Message(HttpStatus.OK.value(), HttpStatus.OK.getReasonPhrase(), message);
	}

}
