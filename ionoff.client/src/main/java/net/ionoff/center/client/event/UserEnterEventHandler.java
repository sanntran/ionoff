package net.ionoff.center.client.event;

import com.google.gwt.event.shared.EventHandler;

/**
 * @author Sann Tran
 */
public interface UserEnterEventHandler extends EventHandler {
	void onUserEnter(UserEnterEvent event);
}