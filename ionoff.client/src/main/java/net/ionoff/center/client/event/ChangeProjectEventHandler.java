package net.ionoff.center.client.event;

import com.google.gwt.event.shared.EventHandler;

public interface ChangeProjectEventHandler extends EventHandler {
	void onChangeProject(ChangeProjectEvent changeItemEvent);
}