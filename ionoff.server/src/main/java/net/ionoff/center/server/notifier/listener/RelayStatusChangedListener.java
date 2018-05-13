package net.ionoff.center.server.notifier.listener;

import java.util.Observable;
import java.util.Observer;

import net.ionoff.center.server.message.event.RelayStatusChangedEvent;
import net.ionoff.center.server.notifier.handler.RelayStatusChangedHandler;

public class RelayStatusChangedListener implements Observer {
	
	private RelayStatusChangedHandler relayStatusChangedHandler;
	
	public RelayStatusChangedListener() {
		// does nothing
	}

	@Override
	public void update(Observable observable, Object event) {
		if (event instanceof RelayStatusChangedEvent) {
			RelayStatusChangedEvent relayStatusChangedEvent = (RelayStatusChangedEvent) event;
			relayStatusChangedHandler.onRelayStatusChanged(relayStatusChangedEvent.getRelay());;
		}
	}

	public void setHandler(RelayStatusChangedHandler handler) {
		this.relayStatusChangedHandler = handler;			
	}
}