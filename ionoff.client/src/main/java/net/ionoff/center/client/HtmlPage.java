package net.ionoff.center.client;

import com.google.gwt.user.client.ui.RootPanel;

public class HtmlPage {
	
	public static void clear() {
		RootPanel.get().clear();
	}
	
	public static RootPanel getRoot() {
		return RootPanel.get();
	}
}
