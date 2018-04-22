package net.ionoff.center.client.base;

import java.util.HashMap;

import com.google.gwt.user.client.ui.HasWidgets;

/**
 * @author Sann Tran
 */
public interface IPresenter {
	public void go();
	public void show(HasWidgets container);
	public String getBaseUrl();
	public HashMap<String, String> getParamsMap(String procedure);
}