package net.ionoff.center.shared.dto;

import com.fasterxml.jackson.annotation.JsonTypeName;

@JsonTypeName( "MediaPlayerDto" )
public class MediaPlayerDto extends DeviceDto {

	private static final long serialVersionUID = 1L;
	public static final String P = "P";
	public static final String XMP = "XMP";
	public static final String IMP = "IMP";
	
	private String mac;
	private String ip;
	private String model;
	
	@Override
	public String getClazz() {
		return "MediaPlayerDto";
	}
	
	public String getMac() {
		return mac;
	}

	public void setMac(String mac) {
		this.mac = mac;
	}
	
	public Boolean isConnected() {
		return false;
	}

	public String getIp() {
		return ip;
	}

	public void setIp(String ip) {
		this.ip = ip;
	}

	public String getModel() {
		return model;
	}

	public void setModel(String model) {
		this.model = model;
	}

	@Override
	public String getStyle() {
		return "mediaPlayer";
	}
}
