package net.ionoff.center.client.event;

import com.google.gwt.event.shared.EventHandler;

/**
 * @author Sann Tran
 */
public interface ShowLoadingEventHandler extends EventHandler {
	void onShowLoading(ShowLoadingEvent showLoadingEvent);
}