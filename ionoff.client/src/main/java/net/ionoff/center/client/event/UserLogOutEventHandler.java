package net.ionoff.center.client.event;

import com.google.gwt.event.shared.EventHandler;

/**
 * @author Sann Tran
 */
public interface UserLogOutEventHandler extends EventHandler {
	void onUserLogOut(UserLogOutEvent event);
}