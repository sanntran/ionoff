package net.xapxinh.center.client.player.event;

import com.google.gwt.event.shared.GwtEvent;

/**
 * @author Sann Tran
 */
public class ShowLoadingEvent extends GwtEvent<ShowLoadingEventHandler> {
	public static Type<ShowLoadingEventHandler> TYPE = new Type<ShowLoadingEventHandler>();

	private boolean loading;

	public ShowLoadingEvent(boolean loading_) {
		this.loading = loading_;
	}

	public boolean isLoading() {
		return this.loading;
	}

	@Override
	public Type<ShowLoadingEventHandler> getAssociatedType() {
		return TYPE;
	}

	@Override
	protected void dispatch(ShowLoadingEventHandler handler) {
		handler.onShowLoading(this);
	}
}
