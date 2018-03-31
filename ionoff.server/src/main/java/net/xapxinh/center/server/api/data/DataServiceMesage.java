package net.xapxinh.center.server.api.data;

public class DataServiceMesage {

	private String status;
	private String message;
	private String clazz;

	public DataServiceMesage() {
		//
	}

	public DataServiceMesage(String status, String message) {
		this.setStatus(status);
		this.setMessage(message);
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getClazz() {
		return clazz;
	}

	public void setClazz(String clazz) {
		this.clazz = clazz;
	}
}
