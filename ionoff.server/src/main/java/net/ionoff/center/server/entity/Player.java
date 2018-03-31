package net.ionoff.center.server.entity;

public class Player extends Device {

	private static final long serialVersionUID = 1L;
	
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

	@Override
	public Boolean getStatus() {
		if (getTime() == null || (System.currentTimeMillis() - getTime().getTime()) > 60000) {
			return false;
		}
		return true;
	}
	
	public boolean isOnline() {
		return getStatus().booleanValue();
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
}
