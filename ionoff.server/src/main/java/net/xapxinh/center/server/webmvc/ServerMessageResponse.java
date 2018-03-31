package net.xapxinh.center.server.webmvc;

import java.io.Serializable;

public class ServerMessageResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	private int status;
	private String message;

	public ServerMessageResponse(int status, String message) {
		this.status = status;
		this.message = message;
	}

	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.status = status;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}
}
