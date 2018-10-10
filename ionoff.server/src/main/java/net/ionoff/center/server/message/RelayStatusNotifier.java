package net.ionoff.center.server.message;

import java.util.Observable;
import java.util.Observer;

import net.ionoff.center.server.message.event.RelayStatusChangedEvent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

@Component
public class RelayStatusNotifier extends Observable {

	@Autowired
	public RelayStatusNotifier(@Qualifier("relayStatusChangedListener") Observer observer) {
		addObserver(observer);
	}
	
	public void notifyListeners(RelayStatusChangedEvent event) {
		setChanged();
		notifyObservers(event);
	}
}