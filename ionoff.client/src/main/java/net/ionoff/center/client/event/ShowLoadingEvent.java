package net.ionoff.center.client.event;

import com.google.gwt.event.shared.GwtEvent;

/**
 * @author Sann Tran
 */
public class ShowLoadingEvent extends GwtEvent<ShowLoadingEventHandler> {
	public static Type<ShowLoadingEventHandler> TYPE = new Type<ShowLoadingEventHandler>();

	private boolean loading;
	private static ShowLoadingEvent instance;
	
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

	public static ShowLoadingEvent getInstance(boolean loading) {
		if (instance == null) {
			instance= new ShowLoadingEvent();
		}
		instance.loading = loading;
		return instance;
	}
}
