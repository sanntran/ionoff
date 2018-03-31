package net.ionoff.center.server.notifier.listener;

import java.util.Observable;
import java.util.Observer;

import org.springframework.beans.factory.annotation.Autowired;

import net.ionoff.center.server.notifier.event.SensorStatusChangedEvent;
import net.ionoff.center.server.notifier.handler.SensorStatusChangedHandler;

public class SensorStatusChangedListener implements Observer {
	
	@Autowired
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
}