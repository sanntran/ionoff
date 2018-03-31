package net.xapxinh.center.client.player.event;

import com.google.gwt.event.shared.GwtEvent;

/**
 * @author Sann Tran
 */
public class ShowPlayerMenuEvent extends GwtEvent<ShowPlayerMenuEventHandler> {
	
	public static Type<ShowPlayerMenuEventHandler> TYPE = new Type<ShowPlayerMenuEventHandler>();
	
	@Override
	public Type<ShowPlayerMenuEventHandler> getAssociatedType() {
		return TYPE;
	}

	@Override
	protected void dispatch(ShowPlayerMenuEventHandler handler) {
		handler.onShowPlayerMenu(this);
	}
}
