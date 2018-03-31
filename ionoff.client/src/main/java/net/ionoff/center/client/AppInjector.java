package net.ionoff.center.client;

import com.google.gwt.core.client.GWT;
import com.google.gwt.inject.client.GinModules;
import com.google.gwt.inject.client.Ginjector;

@GinModules(AppInjectorModule.class)
public interface AppInjector extends Ginjector {
	
    public static final AppInjector INSTANCE = GWT.create(AppInjector.class);

    public IClientApp getClientApp();
}
