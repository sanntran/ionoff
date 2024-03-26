package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Getter
@Setter
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public abstract class Controller implements IEntity {

	public static final Map<String, Class<? extends Controller>> MODELS = new HashMap<>();
	private static final long serialVersionUID = 1L;

	public static final int KEY_LENGTH = 8;
	public static final int ONE_SECOND = 1;

	@EqualsAndHashCode.Include
	private long id;
	private String name;
	private String ip;
	private Integer port;
	private String key;
	private Long connectedTime;
	private Integer crashCount;
	private Project project;
	private List<Relay> relays;
	private List<Sensor> sensors;

	public void overrideKey() {
		// does nothing by default
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
			if (r.getIndex() != null && r.getIndex() == relayIndex) {
				return r;
			}
		}
		return null;
	}


	public boolean isValidKey() {
		return key != null && !key.trim().isEmpty();
	}

	@Override
	public String toString() {
		return "Id: " + getId() + ", Name: " + getName() + ", Key: " + getKey() + ", Model: " + getModel();
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
