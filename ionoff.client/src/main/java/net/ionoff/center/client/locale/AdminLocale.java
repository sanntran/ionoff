package net.ionoff.center.client.locale;


/**
 * @author Sann Tran
 */
import com.google.gwt.core.client.GWT;

public class AdminLocale extends ClientLocale {
	private static AdminConstants adminConsts = GWT.create(AdminConstants.class);
	private static AdminMessages adminMessages = GWT.create(AdminMessages.class);

	public static AdminConstants getAdminConst() {
		return adminConsts;
	}
	public static AdminMessages getAdminMessages() {
		return adminMessages;
	}
}
