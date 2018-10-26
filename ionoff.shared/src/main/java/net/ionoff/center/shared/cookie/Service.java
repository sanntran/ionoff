package net.ionoff.center.shared.cookie;

public class Service {

	public static final String HOST = "host";
	public static final String ENABLED = "enabled";

	private String host;
	private boolean enabled;

	public Service() {}

	public Service(String host, boolean enabled) {
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
