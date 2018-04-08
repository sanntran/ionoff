package net.ionoff.center.server.thread;

public class RelayDriverMqttPayload {
	
	public static final String ID = "id";
	public static final String CODE = "code";
	public static final String IO = "io";
	public static final String CF = "cf";
	public static final String STATUS = "status";
	public static final String RESET = "reset";
	public static final String CRASH = "crash";
	public static final String HELLO = "hello";
	public static final String CHANGED = "changed";
	
	private String id;
	private String code;
	private String io;
	private String cf;
	private String in;
	private String out;
	
	public RelayDriverMqttPayload(String payload) {
		String[] params = payload.split("&");
		for (String param : params) {
			String[] pairs = param.split("=");
			if ("id".equals(pairs[0])) {
				id = pairs[1];
			}
			else if ("code".equals(pairs[0])) {
				code = pairs[1];
			}
			else if ("io".equals(pairs[0])) {
				io = pairs[1];
				String[] inout = io.split(",");
				in = inout[0];
				out = inout[1];
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
	public String getIo() {
		return io;
	}
	public void setIo(String io) {
		this.io = io;
	}
	public String getCf() {
		return cf;
	}
	public void setCf(String cf) {
		this.cf = cf;
	}

	public String getIn() {
		return in;
	}
	
	public String getOut() {
		return out;
	}
}
