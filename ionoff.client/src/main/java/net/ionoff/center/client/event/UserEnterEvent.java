package net.ionoff.center.client.event;

import com.google.gwt.event.shared.GwtEvent;

/**
 * @author Sann Tran
 */
public class UserEnterEvent extends GwtEvent<UserEnterEventHandler> {
	public static Type<UserEnterEventHandler> TYPE = new Type<UserEnterEventHandler>();

	@Override
	public Type<UserEnterEventHandler> getAssociatedType() {
		return TYPE;
	}

	@Override
	protected void dispatch(UserEnterEventHandler handler) {
		handler.onUserEnter(this);
	}
}
