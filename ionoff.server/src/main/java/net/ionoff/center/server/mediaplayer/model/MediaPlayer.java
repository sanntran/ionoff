package net.ionoff.center.server.mediaplayer.model;

import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.EntityUtil;

import java.io.Serializable;

public class MediaPlayer implements Serializable {

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

	public static MediaPlayer fromPlayer(Device device) {
		net.ionoff.center.server.entity.MediaPlayer player = (net.ionoff.center.server.entity.MediaPlayer) EntityUtil.castUnproxy(device, Device.class);
		MediaPlayer p = new MediaPlayer();
		p.setId(player.getId());
		p.setName(player.getName());
		p.setMac(player.getMac());
		p.setIp(player.getIp());
		p.setPort(player.getPort());
		p.setModel(player.getModel());
		p.setConnectedTime(player.getTime().getTime());
		return p;
	}

	public long getId() {
		return id;
	}

	public boolean isOnline() {
		return System.currentTimeMillis() - connectedTime < 35000;
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
