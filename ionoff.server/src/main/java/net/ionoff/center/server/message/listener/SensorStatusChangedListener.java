package net.ionoff.center.server.message.listener;

import java.util.Observable;
import java.util.Observer;

import net.ionoff.center.server.message.event.SensorStatusChangedEvent;
import net.ionoff.center.server.message.handler.SensorStatusChangedHandler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

@Component
@Qualifier("sensorStatusChangedListener")
public class SensorStatusChangedListener implements Observer {

	@Autowired
	private SensorStatusChangedHandler sensorStatusChangedHandler;

	@Override
	public void update(Observable observable, Object event) {
		if (event instanceof SensorStatusChangedEvent) {
			SensorStatusChangedEvent sensorStatusChangedEvent = (SensorStatusChangedEvent)event;
			sensorStatusChangedHandler.onSensorStatusChanged(sensorStatusChangedEvent.getSensor());
		}
	}
}