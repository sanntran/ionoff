package net.ionoff.center.client.common;

import java.util.HashMap;

import com.google.gwt.core.client.GWT;
import com.google.gwt.event.shared.HandlerManager;

import net.ionoff.center.client.utils.ClientUtil;

/**
 * @author Sann Tran
 */
public abstract class AbstractPresenter implements IPresenter {
	
	protected final HandlerManager eventBus;
	
	public AbstractPresenter(HandlerManager eBus) {
		eventBus = eBus;
	}

	@Override
	public HashMap<String, String> getParamsMap(String procedure) {
		return ClientUtil.getParamsMap(eventBus, procedure);
	}
	

	@Override
	public String getBaseUrl() {
		return GWT.getModuleBaseURL().replaceAll((GWT.getModuleName() + "/"), "");
	}
	
	public native void scrollToTop() /*-{
		$wnd.scroll(0, 0);
	}-*/;
}