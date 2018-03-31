package net.ionoff.center.client.event;

import com.google.gwt.event.shared.EventHandler;

/**
 * @author Sann Tran
 */
public interface ShowMessageEventHandler extends EventHandler {
	void onShowMessage(ShowMessageEvent showMessageEvent);
}