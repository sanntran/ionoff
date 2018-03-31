package net.ionoff.center.client.event;

import com.google.gwt.event.shared.EventHandler;

/**
 * @author Sann Tran
 */
public interface ChangeItemEventHandler extends EventHandler {
	void onChangeItem(ChangeItemEvent changeItemEvent);
}