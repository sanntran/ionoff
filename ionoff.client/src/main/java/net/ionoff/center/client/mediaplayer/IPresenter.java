package net.ionoff.center.client.mediaplayer;

import com.google.gwt.user.client.ui.HasWidgets;

/**
 * @author Sann Tran
 */
public interface IPresenter {
	void go();
	void show(HasWidgets container);
	String getBaseUrl();
}