package net.xapxinh.center.client.player;

import com.google.gwt.core.client.GWT;
import com.google.gwt.event.shared.HandlerManager;

/**
 * @author Sann Tran
 */
public abstract class AbstractPresenter implements IPresenter {

	protected final HandlerManager eventBus;

	public AbstractPresenter(HandlerManager eBus) {
		eventBus = eBus;
	}

	@Override
	public String getBaseUrl() {
		return GWT.getModuleBaseURL().replaceAll((GWT.getModuleName() + "/"), "");
	}
}