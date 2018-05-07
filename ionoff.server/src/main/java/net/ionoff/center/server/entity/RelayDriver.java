package net.ionoff.center.server.entity;

import java.util.List;

import net.ionoff.center.shared.entity.RelayDriverModel;

public class RelayDriver extends BaseObj {
	
	private static final long serialVersionUID = 1L;
	
	public static final int KEY_LENGTH = 8;
	public static final String DEFAULT_KEY = "A3000000";
	
	private String ip;
	private Integer port;
	private String key;
	private String model;
	private Long connectedTime;
	private Integer crashCount;
	private Project project;
	private List<Relay> relays;
	private List<Switch> switchs;
	
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
	
	public Long getConnectedTime() {
		return connectedTime;
	}
	public void setConnectedTime(Long connectedTime) {
		this.connectedTime = connectedTime;
	}
	
	public Integer getCrashCount() {
		return crashCount;
	}
	public void setCrashCount(Integer crashCount) {
		this.crashCount = crashCount;
	}
	
	public List<Relay> getRelays() {
		return relays;
	}
	public void setRelays(List<Relay> relays) {
		this.relays = relays;
	}
	
	public String getModel() {
		return model;
	}	
	public void setModel(String model) {
		this.model = model;
	}
	
	public Project getProject() {
		return project;
	}	
	public void setProject(Project project) {
		this.project = project;
	}
	
	public boolean isConnected() {
		if (RelayDriverModel.IONOFF_E4.toString().equals(model)) {
			if (connectedTime == null || System.currentTimeMillis() - connectedTime > 49000) {
				return false;
			}
		}
		if (RelayDriverModel.IONOFF_P4.toString().equals(model) 
				|| RelayDriverModel.IONOFF_P8.toString().equals(model) 
				) {
			if (connectedTime == null || System.currentTimeMillis() - connectedTime > 32000) {
				return false;
			}
		}
		else if (RelayDriverModel.HBQ_EC100.toString().equals(model) 
				|| RelayDriverModel.HLAB_EP2.toString().equals(model)) {
			if (connectedTime == null || System.currentTimeMillis() - connectedTime > 15000) {
				return false;
			}
		}
		return true;
	}
	
	public RelayDriverModel getModelObj() {
		return RelayDriverModel.fromString(model);
	}
	
	public Relay getRelayByIdx(int relayIndex) {
		if (relays == null || relays.isEmpty()) {
			return null;
		}
		for (Relay r : relays) {
			if (r.getIndex() != null && r.getIndex().intValue() == relayIndex) {
				return r;
			}
		}
		return null;
	}
	
	public List<Switch> getSwitchs() {
		return switchs;
	}
	public void setSwitchs(List<Switch> switchs) {
		this.switchs = switchs;
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
}
