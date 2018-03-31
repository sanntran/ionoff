package net.ionoff.center.client.event;

import com.google.gwt.event.shared.GwtEvent;

/**
 * @author Sann Tran
 */
public class ChangeTokenEvent extends GwtEvent<ChangeTokenEventHandler> {
	public static Type<ChangeTokenEventHandler> TYPE = new Type<ChangeTokenEventHandler>();

	private String token;

	public ChangeTokenEvent(String token) {
		this.token = token;
	}

	public String getToken() {
		return this.token;
	}

	@Override
	public Type<ChangeTokenEventHandler> getAssociatedType() {
		return TYPE;
	}

	@Override
	protected void dispatch(ChangeTokenEventHandler handler) {
		handler.onChangeToken(this);
	}
}
