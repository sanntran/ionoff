package net.ionoff.center.server.entity;

import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
public class SensorDriver extends Device {

	private static final long serialVersionUID = 1L;

	private static final long ONLINE_BUFFER = 60000;

	private String mac;
	private String model;
	private List<Sensor> sensors;

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

    public boolean hasSensor() {
		return sensors != null && !sensors.isEmpty();
    }
}
