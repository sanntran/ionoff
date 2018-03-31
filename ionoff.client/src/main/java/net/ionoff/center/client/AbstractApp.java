package net.ionoff.center.client;

import java.util.HashMap;

import com.google.gwt.core.client.GWT;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.inject.Inject;

import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.client.utils.TokenUtil;

/**
 * @author Sann Tran
 */
public abstract class AbstractApp implements IClientApp, ValueChangeHandler<String> {

	protected IRpcServiceProvider rpcProvider;

	@Inject
	protected HandlerManager eventBus;

	@Inject
	protected IAppEventHandler eventHandler;

	public AbstractApp(IRpcServiceProvider rpcProvider) {
		this.rpcProvider = rpcProvider;
	}

	@Override
	public void onValueChange(ValueChangeEvent<String> event) {
		if (TokenUtil.getSourceChangeTokenEvent() == true) {
			handleHistoryTokenChanged();
		}
	}

	@Override
	public void go() {
		eventHandler.bind();
		show(RootPanel.get());
	}

	@Override
	public HashMap<String, String> getParamsMap(String procedure) {
		return ClientUtil.getParamsMap(eventBus, procedure);
	}

	@Override
	public String getBaseUrl() {
		return GWT.getModuleBaseURL().replaceAll((GWT.getModuleName() + "/"), "");
	}
}
