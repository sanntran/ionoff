package net.ionoff.player.connection;

public class MqttResponseMessage {

	private String status;
	private String message;
	private Object object;
	private String clazz;	

	public MqttResponseMessage(String message, Object obj) {
		this.message = message;
		this.clazz = obj.getClass().getSimpleName();
		if (obj instanceof Exception) {			
			this.status = "error";
			this.object = ((Exception) obj).getMessage();
		}
		else {
			this.status = "success";
			this.object = obj;
		}
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

	public Object getObject() {
		return object;
	}

	public void setObject(Object object) {
		this.object = object;
	}

	public String getClazz() {
		return clazz;
	}

	public void setClazz(String clazz) {
		this.clazz = clazz;
	}
}
