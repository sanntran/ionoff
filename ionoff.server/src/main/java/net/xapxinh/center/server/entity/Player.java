package net.xapxinh.center.server.entity;

import java.io.Serializable;

public class Player implements Serializable {

	private static final long serialVersionUID = 1L;
	public static final String P = "P";

	private long id;
	private String ip;
	private Integer port;
	private String mac;
	private String name;
	private String model;
	private String version;
	private String password;
	private Long connectedTime;

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getIp() {
		return ip;
	}

	public void setIp(String ip) {
		this.ip = ip;
	}

	public Integer getPort() {
		return port;
	}

	public void setPort(Integer port) {
		this.port = port;
	}

	public String getMac() {
		return mac;
	}

	public void setMac(String mac) {
		this.mac = mac;
	}

	public String getVersion() {
		return version;
	}

	public void setVersion(String version) {
		this.version = version;
	}

	public Long getConnectedTime() {
		return connectedTime;
	}

	public void setConnectedTime(Long connectedTime) {
		this.connectedTime = connectedTime;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public static String getName(long playerId) {
		return P + playerId;
	}

	public static Long getId(String playerName) {
		return Long.parseLong(playerName.substring(1));
	}

	public String getModel() {
		return model;
	}

	public void setModel(String model) {
		this.model = model;
	}
}
