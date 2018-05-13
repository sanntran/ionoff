package net.ionoff.center.server.notifier;

import java.util.Observable;
import java.util.Observer;

import net.ionoff.center.server.message.event.SensorStatusChangedEvent;

public class SensorStatusNotifier extends Observable {

	public SensorStatusNotifier(Observer ...observers ) {
		for (Observer observer : observers) {
			addObserver(observer);
		}
	}
	
	public void notifyListeners(SensorStatusChangedEvent event) {
		setChanged();
		notifyObservers(event);
	}
}