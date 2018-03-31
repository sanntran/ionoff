package net.ionoff.center.client.event;

import com.google.gwt.event.shared.EventHandler;

/**
 * @author Sann Tran
 */
public interface ChangeTokenEventHandler extends EventHandler {
	void onChangeToken(ChangeTokenEvent event);
}