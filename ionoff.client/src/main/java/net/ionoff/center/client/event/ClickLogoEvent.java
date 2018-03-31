package net.ionoff.center.client.event;

import com.google.gwt.event.shared.GwtEvent;

/**
 * @author Sann Tran
 */
public class ClickLogoEvent extends GwtEvent<ClickLogoEventHandler> {
	public static Type<ClickLogoEventHandler> TYPE = new Type<ClickLogoEventHandler>();

	@Override
	public Type<ClickLogoEventHandler> getAssociatedType() {
		return TYPE;
	}

	@Override
	protected void dispatch(ClickLogoEventHandler handler) {
		handler.onClickLogo(this);
	}
}
