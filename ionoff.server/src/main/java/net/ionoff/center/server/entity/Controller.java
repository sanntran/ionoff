package net.ionoff.center.server.entity;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.*;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Getter
@Setter
@Entity
@Table(name = "controllers")
@EqualsAndHashCode(onlyExplicitlyIncluded = true)
public class Controller implements IEntity {

	public static final Map<String, Class<? extends Controller>> MODELS = new HashMap<>();
	private static final long serialVersionUID = 1L;

	public static final int ONE_SECOND = 1;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column
	@EqualsAndHashCode.Include
	private long id;

	@Column
	private String name;

	@Column
	private String type;

	@Column
	private String ip;

	@Column
	private Integer port;

	@Column(name = "code")
	private String key;

	@Column
	private String protocol;

	@Column
	private Integer input;

	@Column
	private Integer output;

	@Column
	private String model;

	@Column
	private Integer onlineBuffer;

	@Column
	private Long lastConnected;

	@Column
	private Long connectionExpired;

	@Column
	private Integer crashCount;

	@OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
	@JoinColumn(name = "driver_id", referencedColumnName = "id")
	@OrderColumn(name = "idx")
	private List<Relay> relays;

	@OneToMany(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
	@JoinColumn(name = "controller_id", referencedColumnName = "id")
	@OrderColumn(name = "idx")
	private List<Sensor> sensors;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "project_id", referencedColumnName = "id")
	private Project project;

	public void overrideKey() {
		// does nothing by default
	}

	public boolean isConnected() {
		if (lastConnected == null || System.currentTimeMillis() - lastConnected > getOnlineBuffer()) {
			return false;
		}
		return true;
	}

	public boolean isOffline() {
		return !isConnected();
	}

	public boolean isOnline() {
		return isConnected();
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

	/**
	 * Lazy: not auto-publish
	 */
	public boolean isLazy() {
		return false;
	}

	public boolean autoRevert() {
		return false;
	}

	public String getCommandStatus() {
		return "{ioget}";
	}

	public String getCommandOpenRelay(int relayIndex) {
		return "{ioseto" + (relayIndex + 1) + "1}";
	}

	public String getCommandCloseRelay(int relayIndex) {
		return "{ioseto" + (relayIndex + 1) + "0}";
	}

	public String getCommandOpenRelay(int relayIndex, Integer autoRevert) {
		int revert = autoRevert != null && autoRevert > 0 ? autoRevert : 0;
		return "{ioseto" + (relayIndex + 1) + "1"+ revert + "}";
	}

	public String getCommandCloseRelay(int relayIndex, Integer autoRevert) {
		int revert = autoRevert != null && autoRevert > 0 ? autoRevert : 0;
		return "{ioseto" + (relayIndex + 1) + "0" + revert + "}";
	}

	public int getOneSecondDelay() {
		return 650;
	}
	@Override
	public String toString() {
		return "Id: " + getId() + ", Name: " + getName() + ", Key: " + getKey() + ", Model: " + getModel();
	}

}
