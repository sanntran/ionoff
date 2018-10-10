package net.ionoff.center.server.message;

import java.util.Observable;
import java.util.Observer;

import net.ionoff.center.server.message.event.SensorStatusChangedEvent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;


@Component
public class SensorStatusNotifier extends Observable {

	@Autowired
	public SensorStatusNotifier(@Qualifier("sensorStatusChangedListener") Observer observer ) {
		addObserver(observer);
	}
	
	public void notifyListeners(SensorStatusChangedEvent event) {
		setChanged();
		notifyObservers(event);
	}
}