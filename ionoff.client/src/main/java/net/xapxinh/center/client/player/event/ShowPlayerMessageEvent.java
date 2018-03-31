package net.xapxinh.center.client.player.event;

import com.google.gwt.event.shared.GwtEvent;

/**
 * @author Sann Tran
 */
public class ShowPlayerMessageEvent extends GwtEvent<ShowPlayerMessageEventHandler> {
	
	public static final int ERROR = 0;
	public static final int NORMAL = 1;
	public static final int SUCCESS = 2;
	public static final int WARNING = 3;
	public static Type<ShowPlayerMessageEventHandler> TYPE = new Type<ShowPlayerMessageEventHandler>();

	private final String message;
	private final int mesageType;

	public ShowPlayerMessageEvent(String message, int type) {
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
	public Type<ShowPlayerMessageEventHandler> getAssociatedType() {
		return TYPE;
	}

	@Override
	protected void dispatch(ShowPlayerMessageEventHandler handler) {
		handler.onShowMessage(this);
	}
}
