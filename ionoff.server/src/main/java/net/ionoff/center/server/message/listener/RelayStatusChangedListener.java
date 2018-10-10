package net.ionoff.center.server.message.listener;

import java.util.Observable;
import java.util.Observer;

import net.ionoff.center.server.message.event.RelayStatusChangedEvent;
import net.ionoff.center.server.message.handler.RelayStatusChangedHandler;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

@Component
@Qualifier("relayStatusChangedListener")
public class RelayStatusChangedListener implements Observer {

	@Autowired
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

}