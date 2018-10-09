package net.ionoff.center.server.entity;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class RelayDriver extends BaseObj {

	public static final Map<String, Class<? extends RelayDriver>> MODELS = new HashMap<>();

	private static final long serialVersionUID = 1L;

	public static final int KEY_LENGTH = 8;
	public static final int ONE_SECOND = 1; // second

	private String ip;
	private Integer port;
	private String key;
	private Long connectedTime;
	private Integer crashCount;
	private Project project;
	private List<Relay> relays;
	private List<Switch> switchs;

	public String getIp() {
		return ip;
	}

	public void overrideKey() {
		// does nothing by default
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


	public Project getProject() {
		return project;
	}
	public void setProject(Project project) {
		this.project = project;
	}

	public boolean isConnected() {
		if (connectedTime == null || System.currentTimeMillis() - connectedTime > getOnlineBuffer()) {
			return false;
		}
		return true;
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
		return key != null && !key.trim().isEmpty();
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();

		builder.append(super.toString())
				.append(", Key: ").append(key)
				.append(", Model: ").append(getModel());

		return builder.toString();
	}

	/**
	 * Lazy: not auto-publish
	 */
	public boolean isLazy() {
		return false;
	}

	public boolean autoRevert() {
		return false;
	}

	public abstract int getInput();

	public abstract int getOutput();

	public abstract String getModel();

	public abstract int getOnlineBuffer();

	public abstract String getProtocol();

	public abstract String getCommandStatus();

	public abstract String getCommandOpenRelay(int relayIndex);

	public abstract String getCommandCloseRelay(int relayIndex);

	public abstract String getCommandOpenRelay(int relayIndex, Integer autoRevert);

	public abstract String getCommandCloseRelay(int relayIndex, Integer autoRevert);

	public abstract int getOneSecondDelay();
}
