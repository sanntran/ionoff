package net.ionoff.center.client.locale;

import com.google.gwt.i18n.client.Messages;

/**
 * @author Sann Tran
 */
public interface ProjectMessages extends Messages {
	
	@DefaultMessage("Hoàn tất kịch bản ''{0}''")
	String performedScene(String sceneName);
}
