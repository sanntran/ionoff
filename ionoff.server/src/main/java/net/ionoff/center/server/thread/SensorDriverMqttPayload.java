package net.ionoff.center.server.thread;

public class SensorDriverMqttPayload {
	
	public static final String ID = "id";
	public static final String CODE = "code";
	public static final String STATUS = "status";
	public static final String RESET = "reset";
	public static final String CRASH = "crash";
	public static final String HELLO = "hello";
	public static final String CHANGED = "changed";
	
	private String id;
	private String code;
	private String status;
	private String cf;
	private Double value;
	private Long index;
	
	public SensorDriverMqttPayload(String payload) {
		String[] params = payload.split("&");
		for (String param : params) {
			String[] pairs = param.split("=");
			if ("id".equals(pairs[0])) {
				id = pairs[1];
			}
			else if ("code".equals(pairs[0])) {
				code = pairs[1];
			}
			else if ("status".equals(pairs[0])) {
				setStatus(pairs[1]);
				String vals[] = status.split(",");
				if (vals.length == 2) {
					try {
						value = Double.valueOf(vals[0]);
					}
					catch (Exception e) {
						// ignore
					}
					try {
						index = Long.valueOf(vals[1]);
					}
					catch (Exception e) {
						// ignore
					}
				}
			}
			else if ("cf".equals(pairs[0])) {
				cf = pairs[1];
			}
		}
	}
	
	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	
	public String getCode() {
		return code;
	}
	public void setCode(String code) {
		this.code = code;
	}
	public String getCf() {
		return cf;
	}
	public void setCf(String cf) {
		this.cf = cf;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public Long getIndex() {
		return index;
	}
	public Double getValue() {
		return value;
	}

}
