package net.ionoff.center.client.mediaplayer.event;

import com.google.gwt.event.shared.EventHandler;

/**
 * @author Sann Tran
 */
public interface ShowPlayerMessageEventHandler extends EventHandler {
	void onShowMessage(ShowPlayerMessageEvent showMessageEvent);
}