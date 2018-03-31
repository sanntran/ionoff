package net.ionoff.center.client.event;

import com.google.gwt.event.shared.GwtEvent;

/**
 * @author Sann Tran
 */
public class UserLogOutEvent extends GwtEvent<UserLogOutEventHandler> {
	public static Type<UserLogOutEventHandler> TYPE = new Type<UserLogOutEventHandler>();

	@Override
	public Type<UserLogOutEventHandler> getAssociatedType() {
		return TYPE;
	}

	@Override
	protected void dispatch(UserLogOutEventHandler handler) {
		handler.onUserLogOut(this);
	}
}
