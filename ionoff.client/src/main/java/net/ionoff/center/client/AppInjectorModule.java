package net.ionoff.center.client;

import javax.inject.Singleton;

import org.fusesource.restygwt.client.RestServiceProxy;

import com.google.gwt.core.client.GWT;
import com.google.gwt.event.shared.HandlerManager;
import com.google.inject.Provider;

import net.ionoff.center.client.service.LoginService;
import net.ionoff.center.client.service.ApiServiceUrl;
import net.xapxinh.center.client.player.rpc.PlayerService;

public class AppInjectorModule extends AbstractInjectorModule {
    
	@Override
    protected void configure() {
    	super.configure();
        bind(IAppController.class).to(AppControllerImpl.class).in(Singleton.class); 
        bind(IAppEventHandler.class).to(AppEventHandler.class).in(Singleton.class);
        bind(IClientApp.class).to(Application.class).in(Singleton.class);
        
        bind(RestyGwtConfig.class).asEagerSingleton();
    }
    
    public static class HandlerManagerProvider implements Provider<HandlerManager> {
		@Override
		public HandlerManager get() {
			return new HandlerManager(null);
		}    	
    }
        
    public static class LoginServiceProvider implements Provider<LoginService> {
		@Override
		public LoginService get() {			
			final org.fusesource.restygwt.client.Resource resource
			= new org.fusesource.restygwt.client.Resource(ApiServiceUrl.get());
			final LoginService service = GWT.create(LoginService.class);
			((RestServiceProxy)service).setResource(resource);
			return service;
		}    	
    }
    
    public static class PlayerRpcProvider implements Provider<PlayerService> {
		@Override
		public PlayerService get() {
			final org.fusesource.restygwt.client.Resource resource
			= new org.fusesource.restygwt.client.Resource(ApiServiceUrl.get());
			final PlayerService service = GWT.create(PlayerService.class);
			((RestServiceProxy)service).setResource(resource);
			return service;
		}    	
    }
}
