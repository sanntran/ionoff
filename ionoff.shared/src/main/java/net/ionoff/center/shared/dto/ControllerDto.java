package net.ionoff.center.shared.dto;

import java.util.List;

public class ControllerDto extends BaseDto {

	private static final long serialVersionUID = 1L;
	public static final String DEFAULT_KEY = "A3000000";
	
	private String ip;
	private Integer port;
	private String key;
	private String model;
	private Long projectId;
	private List<RelayDto> relays;
	private Boolean isOnline;
	
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
	
	public String getKey() {
		return key;
	}
	public void setKey(String key) {
		this.key = key;
	}
	
	public String getModel() {
		return model;
	}

	public void setModel(String model) {
		this.model = model;
	}
	
	public Long getProjectId() {
		return projectId;
	}
	public void setProjectId(Long projectId) {
		this.projectId = projectId;
	}

	public boolean isValidKey() {
		return key != null && !key.trim().isEmpty() 
				&& !DEFAULT_KEY.equals(key);
	}
	
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		
		builder.append(super.toString())
				.append(", Key: ").append(key)
				.append(", Model: ").append(model);
		
		return builder.toString();
	}
	
	public List<RelayDto> getRelays() {
		return relays;
	}
	
	public void setRelays(List<RelayDto> relays) {
		this.relays = relays;
	}
	
	public Boolean getIsOnline() {
		return isOnline;
	}
	public void setIsOnline(Boolean isOnline) {
		this.isOnline = isOnline;
	}
}
