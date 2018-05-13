package net.ionoff.center.server.notifier;

import java.util.Observable;
import java.util.Observer;

import net.ionoff.center.server.message.event.RelayStatusChangedEvent;

public class RelayStatusNotifier extends Observable {

	public RelayStatusNotifier(Observer ...observers ) {
		for (Observer observer : observers) {
			addObserver(observer);
		}
	}
	
	public void notifyListeners(RelayStatusChangedEvent event) {
		setChanged();
		notifyObservers(event);
	}
}