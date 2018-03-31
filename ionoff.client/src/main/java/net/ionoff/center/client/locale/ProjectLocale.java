package net.ionoff.center.client.locale;



/**
 * @author Sann Tran
 */
import com.google.gwt.core.client.GWT;

public class ProjectLocale extends ClientLocale {
	private static ProjectConstants projectConsts = GWT.create(ProjectConstants.class);
	private static ProjectMessages projectMessages = GWT.create(ProjectMessages.class);

	public static ProjectConstants getProjectConst() {
		return projectConsts;
	}
	public static ProjectMessages getProjectMessages() {
		return projectMessages;
	}
}
