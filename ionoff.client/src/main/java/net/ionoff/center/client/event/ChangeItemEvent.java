package net.ionoff.center.client.event;

import com.google.gwt.event.shared.GwtEvent;

public class ChangeItemEvent extends GwtEvent<ChangeItemEventHandler> {
	public static Type<ChangeItemEventHandler> TYPE = new Type<ChangeItemEventHandler>();

	public ChangeItemEvent() {
	}

	@Override
	public Type<ChangeItemEventHandler> getAssociatedType() {
		return TYPE;
	}

	@Override
	protected void dispatch(ChangeItemEventHandler handler) {
		handler.onChangeItem(this);
	}
}
