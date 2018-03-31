package net.ionoff.center.client.event;

import com.google.gwt.event.shared.GwtEvent;

/**
 * @author Sann Tran
 */
public class ChangeLanguageEvent extends GwtEvent<ChangeLanguageEventHandler> {
	public static Type<ChangeLanguageEventHandler> TYPE = new Type<ChangeLanguageEventHandler>();

	private String language;

	public ChangeLanguageEvent(String lan) {
		this.language = lan;
	}

	public String getLanguage() {
		return this.language;
	}

	@Override
	public Type<ChangeLanguageEventHandler> getAssociatedType() {
		return TYPE;
	}

	@Override
	protected void dispatch(ChangeLanguageEventHandler handler) {
		handler.onChangeLanguage(this);
	}
}
