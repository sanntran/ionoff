package net.ionoff.center.server.message.event;

import net.ionoff.center.server.entity.Sensor;

public class SensorStatusChangedEvent {
	
	private final Sensor sensor;
	
	public SensorStatusChangedEvent(Sensor sensor) {
		this.sensor = sensor;
	}

	public Sensor getSensor() {
		return sensor;
	}
}
 