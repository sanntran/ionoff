package net.ionoff.center.server.message.listener;

import java.util.Observable;
import java.util.Observer;

import net.ionoff.center.server.message.event.SensorStatusChangedEvent;
import net.ionoff.center.server.message.handler.SensorStatusChangedHandler;

public class SensorStatusChangedListener implements Observer {
	
	private SensorStatusChangedHandler sensorStatusChangedHandler;
	
	public SensorStatusChangedListener() {
		// does nothing
	}

	@Override
	public void update(Observable observable, Object event) {
		if (event instanceof SensorStatusChangedEvent) {
			SensorStatusChangedEvent sensorStatusChangedEvent = (SensorStatusChangedEvent)event;
			sensorStatusChangedHandler.onSensorStatusChanged(sensorStatusChangedEvent.getSensor());
		}
	}

	public void setHandler(SensorStatusChangedHandler handler) {
		this.sensorStatusChangedHandler = handler;
	}
}