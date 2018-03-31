package net.xapxinh.dataservice.webmvc;

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

	public DataServiceMesage(String status, String message, String clazz) {
		this.setStatus(status);
		this.setMessage(message);
		this.setClazz(clazz);
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


	public static String getSuccessMessageJSON() {
		return "{\"status\":\"200\",\"message\":\"Success\"}";
	}

	public String getClazz() {
		return clazz;
	}

	public void setClazz(String clazz) {
		this.clazz = clazz;
	}
}
