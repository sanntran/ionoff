package net.ionoff.center.client.event;

import com.google.gwt.event.shared.GwtEvent;

/**
 * @author Sann Tran
 */
public class ShowMessageEvent extends GwtEvent<ShowMessageEventHandler> {
	public static Type<ShowMessageEventHandler> TYPE = new Type<ShowMessageEventHandler>();

	private String message;
	private int mesageType;

	public static final int ERROR = 0;
	public static final int NORMAL = 1;
	public static final int SUCCESS = 2;
	public static final int WARNING = 3;

	public ShowMessageEvent(String message, int type) {
		this.message = message;
		this.mesageType = type;
	}

	public String getMessage() {
		return this.message;
	}

	public int getMessageType() {
		return this.mesageType;
	}

	@Override
	public Type<ShowMessageEventHandler> getAssociatedType() {
		return TYPE;
	}

	@Override
	protected void dispatch(ShowMessageEventHandler handler) {
		handler.onShowMessage(this);
	}
}
