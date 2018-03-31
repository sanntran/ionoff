package net.ionoff.center.client.event;

import com.google.gwt.event.shared.GwtEvent;

public class FullscreenEvent extends GwtEvent<FullscreenEventHandler> {
	public static Type<FullscreenEventHandler> TYPE = new Type<FullscreenEventHandler>();

	@Override
	public Type<FullscreenEventHandler> getAssociatedType() {
		return TYPE;
	}

	@Override
	protected void dispatch(FullscreenEventHandler handler) {
		handler.onFullscreen(this);
	}
}
