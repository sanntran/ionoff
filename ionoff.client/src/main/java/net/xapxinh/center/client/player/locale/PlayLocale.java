package net.xapxinh.center.client.player.locale;

/**
 * @author Sann Tran
 */
import com.google.gwt.core.client.GWT;

public class PlayLocale {
	private static PlayConstants playerConsts = GWT.create(PlayConstants.class);
	private static PlayerMessages playerMessages = GWT.create(PlayerMessages.class);

	public static PlayConstants getPlayConsts() {
		return playerConsts;
	}
	
	public static PlayerMessages getPlayMessages() {
		return playerMessages;
	}
}
