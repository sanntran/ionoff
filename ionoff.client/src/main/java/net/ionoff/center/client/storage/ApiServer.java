package net.ionoff.center.client.storage;

public class ApiServer {
	
	static final String HOST = "host";
	static final String ENABLED = "enabled";
	
	private String host;
	private boolean enabled;
	
	public ApiServer() {}
	
	public ApiServer(String host, boolean enabled) {
		this.host = host;
		this.enabled = enabled;
	}
	
	public String getHost() {
		return host;
	}
	public void setHost(String host) {
		this.host = host;
	}
	
	public boolean isEnabled() {
		return enabled;
	}
	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}
}
