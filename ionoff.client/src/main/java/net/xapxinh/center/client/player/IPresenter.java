package net.xapxinh.center.client.player;

import com.google.gwt.user.client.ui.HasWidgets;

/**
 * @author Sann Tran
 */
public interface IPresenter {
	void go();
	void show(HasWidgets container);
	String getBaseUrl();
}