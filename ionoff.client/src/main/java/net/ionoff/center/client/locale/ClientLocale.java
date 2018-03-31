package net.ionoff.center.client.locale;

import com.google.gwt.core.client.GWT;

/**
 * @author Sann Tran
 */

public class ClientLocale {

	public static final String vi_VN = "vi";
	public static final String en_EN = "en";
	
	private static ClientConstants clientConsts = GWT.create(ClientConstants.class);
	private static ClientMessages clientmessages = GWT.create(ClientMessages.class);

	public static ClientConstants getClientConst() {
		return clientConsts;
	}

	public static ClientMessages getClientMessage() {
		return clientmessages;
	}
}
