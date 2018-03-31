package net.ionoff.center.client;

import javax.inject.Singleton;

import org.fusesource.restygwt.client.RestService;
import org.fusesource.restygwt.client.RestServiceProxy;

import com.google.gwt.core.client.GWT;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.inject.client.AbstractGinModule;
import com.google.inject.Provider;

import net.ionoff.center.client.service.ApiServiceUrl;
import net.ionoff.center.client.service.AreaService;
import net.ionoff.center.client.service.ControllerService;
import net.ionoff.center.client.service.DashboardService;
import net.ionoff.center.client.service.DeviceService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.service.LoginService;
import net.ionoff.center.client.service.ModeSceneService;
import net.ionoff.center.client.service.ModeSensorSceneService;
import net.ionoff.center.client.service.ModeSensorService;
import net.ionoff.center.client.service.ModeSensorUserService;
import net.ionoff.center.client.service.ModeService;
import net.ionoff.center.client.service.ProjectService;
import net.ionoff.center.client.service.RelayService;
import net.ionoff.center.client.service.RpcServiceProviderImpl;
import net.ionoff.center.client.service.SceneActionService;
import net.ionoff.center.client.service.ScenePlayerActionService;
import net.ionoff.center.client.service.SceneRelayActionService;
import net.ionoff.center.client.service.SceneService;
import net.ionoff.center.client.service.SchedulePlayerActionService;
import net.ionoff.center.client.service.ScheduleRelayActionService;
import net.ionoff.center.client.service.ScheduleService;
import net.ionoff.center.client.service.SensorService;
import net.ionoff.center.client.service.SystemService;
import net.ionoff.center.client.service.UserService;
import net.ionoff.center.client.service.ZoneService;
import net.xapxinh.center.client.player.rpc.PlayerService;

public class AbstractInjectorModule extends AbstractGinModule {
    
	@Override
    protected void configure() {
    	
        bind(HandlerManager.class).toProvider(HandlerManagerProvider.class).in(Singleton.class);
        
        bind(LoginService.class).toProvider(LoginServiceProvider.class).in(Singleton.class);
        bind(PlayerService.class).toProvider(PlayerServiceProvider.class).in(Singleton.class);
        
        bind(AreaService.class).toProvider(AreaServiceProvider.class).in(Singleton.class);
        bind(ControllerService.class).toProvider(ControllerServiceProvider.class).in(Singleton.class);
        bind(DeviceService.class).toProvider(DeviceServiceProvider.class).in(Singleton.class);
        bind(ProjectService.class).toProvider(ProjectServiceProvider.class).in(Singleton.class);
        bind(ModeSceneService.class).toProvider(ModeSceneServiceProvider.class).in(Singleton.class);
        bind(ModeSensorSceneService.class).toProvider(ModeSensorSceneServiceProvider.class).in(Singleton.class);
        bind(ModeSensorService.class).toProvider(ModeSensorServiceProvider.class).in(Singleton.class);
        bind(ModeSensorUserService.class).toProvider(ModeSensorUserServiceProvider.class).in(Singleton.class);
        bind(ModeService.class).toProvider(ModeServiceProvider.class).in(Singleton.class);
        bind(RelayService.class).toProvider(RelayServiceProvider.class).in(Singleton.class);
        bind(SceneActionService.class).toProvider(SceneActionServiceProvider.class).in(Singleton.class);
        bind(ScenePlayerActionService.class).toProvider(ScenePlayerActionServiceProvider.class).in(Singleton.class);
        bind(SceneRelayActionService.class).toProvider(SceneRelayActionServiceProvider.class).in(Singleton.class);
        bind(SceneService.class).toProvider(SceneServiceProvider.class).in(Singleton.class);
        bind(SchedulePlayerActionService.class).toProvider(SchedulePlayerActionServiceProvider.class).in(Singleton.class);
        bind(ScheduleRelayActionService.class).toProvider(ScheduleRelayActionServiceProvider.class).in(Singleton.class);
        bind(ScheduleService.class).toProvider(ScheduleServiceProvider.class).in(Singleton.class);
        bind(SensorService.class).toProvider(SensorServiceProvider.class).in(Singleton.class);
        bind(ZoneService.class).toProvider(ZoneServiceProvider.class).in(Singleton.class);
        bind(UserService.class).toProvider(UserServiceProvider.class).in(Singleton.class);
        bind(SystemService.class).toProvider(SystemServiceProvider.class).in(Singleton.class);
        bind(DashboardService.class).toProvider(DashboardServiceProvider.class).in(Singleton.class);

        bind(IRpcServiceProvider.class).to(RpcServiceProviderImpl.class).in(Singleton.class); 
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
			final LoginService service = GWT.create(LoginService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    
    public static class PlayerServiceProvider implements Provider<PlayerService> {
		@Override
		public PlayerService get() {
			final PlayerService service = GWT.create(PlayerService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    
    public static class AreaServiceProvider implements Provider<AreaService> {
		@Override
		public AreaService get() {
			final AreaService service = GWT.create(AreaService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class ControllerServiceProvider implements Provider<ControllerService> {
		@Override
		public ControllerService get() {
			final ControllerService service = GWT.create(ControllerService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class DeviceServiceProvider implements Provider<DeviceService> {
		@Override
		public DeviceService get() {
			final DeviceService service = GWT.create(DeviceService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class ProjectServiceProvider implements Provider<ProjectService> {
		@Override
		public ProjectService get() {
			final ProjectService service = GWT.create(ProjectService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class ModeSceneServiceProvider implements Provider<ModeSceneService> {
		@Override
		public ModeSceneService get() {
			final ModeSceneService service = GWT.create(ModeSceneService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class ModeSensorSceneServiceProvider implements Provider<ModeSensorSceneService> {
		@Override
		public ModeSensorSceneService get() {
			final ModeSensorSceneService service = GWT.create(ModeSensorSceneService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class ModeSensorServiceProvider implements Provider<ModeSensorService> {
		@Override
		public ModeSensorService get() {
			final ModeSensorService service = GWT.create(ModeSensorService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class ModeSensorUserServiceProvider implements Provider<ModeSensorUserService> {
		@Override
		public ModeSensorUserService get() {
			final ModeSensorUserService service = GWT.create(ModeSensorUserService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class ModeServiceProvider implements Provider<ModeService> {
		@Override
		public ModeService get() {
			final ModeService service = GWT.create(ModeService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class RelayServiceProvider implements Provider<RelayService> {
		@Override
		public RelayService get() {
			final RelayService service = GWT.create(RelayService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class SceneActionServiceProvider implements Provider<SceneActionService> {
		@Override
		public SceneActionService get() {
			final SceneActionService service = GWT.create(SceneActionService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class ScenePlayerActionServiceProvider implements Provider<ScenePlayerActionService> {
		@Override
		public ScenePlayerActionService get() {
			final ScenePlayerActionService service = GWT.create(ScenePlayerActionService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class SceneRelayActionServiceProvider implements Provider<SceneRelayActionService> {
		@Override
		public SceneRelayActionService get() {
			final SceneRelayActionService service = GWT.create(SceneRelayActionService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class SceneServiceProvider implements Provider<SceneService> {
		@Override
		public SceneService get() {
			final SceneService service = GWT.create(SceneService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class SchedulePlayerActionServiceProvider implements Provider<SchedulePlayerActionService> {
		@Override
		public SchedulePlayerActionService get() {
			final SchedulePlayerActionService service = GWT.create(SchedulePlayerActionService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class ScheduleRelayActionServiceProvider implements Provider<ScheduleRelayActionService> {
		@Override
		public ScheduleRelayActionService get() {
			final ScheduleRelayActionService service = GWT.create(ScheduleRelayActionService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class ScheduleServiceProvider implements Provider<ScheduleService> {
		@Override
		public ScheduleService get() {
			final ScheduleService service = GWT.create(ScheduleService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class SensorServiceProvider implements Provider<SensorService> {
		@Override
		public SensorService get() {
			final SensorService service = GWT.create(SensorService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class ZoneServiceProvider implements Provider<ZoneService> {
		@Override
		public ZoneService get() {
			final ZoneService service = GWT.create(ZoneService.class);
			setRestServiceProxyResource(service);
			return service;
		}    	
    }
    public static class UserServiceProvider implements Provider<UserService> {
		@Override
		public UserService get() {
			final UserService service = GWT.create(UserService.class);
			setRestServiceProxyResource(service);
			return service;
		}
    }
    
    public static class SystemServiceProvider implements Provider<SystemService> {
		@Override
		public SystemService get() {
			final SystemService service = GWT.create(SystemService.class);
			setRestServiceProxyResource(service);
			return service;
		}
    }
    
    public static class DashboardServiceProvider implements Provider<DashboardService> {
		@Override
		public DashboardService get() {
			final DashboardService service = GWT.create(DashboardService.class);
			setRestServiceProxyResource(service);
			return service;
		}
    }

	private static void setRestServiceProxyResource(RestService service) {
		final org.fusesource.restygwt.client.Resource resource
		= new org.fusesource.restygwt.client.Resource(ApiServiceUrl.get());
		((RestServiceProxy)service).setResource(resource);
	} 
}
