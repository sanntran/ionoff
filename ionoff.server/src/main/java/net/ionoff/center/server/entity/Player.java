package net.ionoff.center.server.entity;

public class Player extends Device {

	private static final long serialVersionUID = 1L;

	private static final long ONLINE_BUFFER = 60000;

	public static final String XMP = "XMP";
	public static final String IMP = "IMP";
	
	private String ip;
	private String mac;
	private Integer port;
	private String model;

	public String getIp() {
		return ip;
	}

	public void setIp(String ip) {
		this.ip = ip;
	}

	public String getMac() {
		return mac;
	}

	public void setMac(String mac) {
		this.mac = mac;
	}

	public Integer getPort() {
		return port;
	}

	public void setPort(Integer port) {
		this.port = port;
	}

	public String getModel() {
		return model;
	}

	public void setModel(String model) {
		this.model = model;
	}

	@Override
	public Boolean getStatus() {
		return isOnline();
	}

	public boolean isOnline() {
		if (getTime() == null || (System.currentTimeMillis() - getTime().getTime()) > ONLINE_BUFFER) {
			return false;
		}
		return true;
	}

}
