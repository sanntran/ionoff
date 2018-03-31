package net.ionoff.center.client;

import com.google.gwt.core.client.EntryPoint;

import net.ionoff.center.client.storage.StorageService;

/**
 * Entry point classes define <code>onModuleLoad()</code>.
 */
public class AppEntryPoint implements EntryPoint {
	
	@Override
	public void onModuleLoad() {
		StorageService.getInstance().loadStorage();
		AppInjector.INSTANCE.getClientApp().go();
	}
}
