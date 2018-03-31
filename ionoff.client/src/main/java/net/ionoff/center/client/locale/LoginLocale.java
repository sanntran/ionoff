package net.ionoff.center.client.locale;


import com.google.gwt.core.client.GWT;

public class LoginLocale {
	private static LoginConstants loginConsts = GWT.create(LoginConstants.class);
	private static LoginMessages loginMessages = GWT.create(LoginMessages.class);

	public static LoginConstants getLoginConst() {
		return loginConsts;
	}
	public static LoginMessages getLoginMessages() {
		return loginMessages;
	}
}
