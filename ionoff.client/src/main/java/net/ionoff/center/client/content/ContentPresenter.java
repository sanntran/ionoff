package net.ionoff.center.client.content;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Panel;

import net.ionoff.center.client.area.AreaTablePresenter;
import net.ionoff.center.client.area.AreaTableView;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.dashboard.DashboardPresenter;
import net.ionoff.center.client.dashboard.DashboardView;
import net.ionoff.center.client.device.DeviceListPresenter;
import net.ionoff.center.client.device.DeviceListView;
import net.ionoff.center.client.device.DeviceTablePresenter;
import net.ionoff.center.client.device.DeviceTableView;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.mode.ModeListPresenter;
import net.ionoff.center.client.mode.ModeListView;
import net.ionoff.center.client.mode.ModeTablePresenter;
import net.ionoff.center.client.mode.ModeTableView;
import net.ionoff.center.client.project.ProjectTablePresenter;
import net.ionoff.center.client.project.ProjectTableView;
import net.ionoff.center.client.relay.RelayTablePresenter;
import net.ionoff.center.client.relay.RelayTableView;
import net.ionoff.center.client.controller.ControllerTablePresenter;
import net.ionoff.center.client.controller.ControllerTableView;
import net.ionoff.center.client.scene.SceneListPresenter;
import net.ionoff.center.client.scene.SceneListView;
import net.ionoff.center.client.scene.SceneTablePresenter;
import net.ionoff.center.client.scene.SceneTableView;
import net.ionoff.center.client.schedule.ScheduleTablePresenter;
import net.ionoff.center.client.schedule.ScheduleTableView;
import net.ionoff.center.client.sensor.SensorTablePresenter;
import net.ionoff.center.client.sensor.SensorTableView;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.user.SystemUsersPresenter;
import net.ionoff.center.client.user.UserTablePresenter;
import net.ionoff.center.client.user.UserTableView;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.client.zone.ZoneListPresenter;
import net.ionoff.center.client.zone.ZoneListView;
import net.ionoff.center.client.zone.ZoneTablePresenter;
import net.ionoff.center.client.zone.ZoneTableView;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.MediaPlayerDto;
import net.ionoff.center.client.mediaplayer.PlayerPresenter;
import net.ionoff.center.client.mediaplayer.PlayerView;

public class ContentPresenter extends AbstractPresenter {

    interface Display {
		Panel asPanel();
    }
	private SystemUsersPresenter systemUserPresenter;
	private ProjectTablePresenter systemProjectPresenter;
	private DashboardPresenter dashboardPresenter;
	private DeviceListPresenter deviceListPresenter;
	private DeviceTablePresenter deviceTablePresenter;
	private SceneTablePresenter sceneTablePresenter;
	private SceneListPresenter sceneListPresenter;
	private ScheduleTablePresenter scheduleTablePresenter;
	private ModeTablePresenter modeTablePresenter;
	private ModeListPresenter modeListPresenter;
	private ControllerTablePresenter controllerTablePresenter;
	private RelayTablePresenter relayTablePresenter;
	private UserTablePresenter userTablePresenter;
	private SensorTablePresenter sensorTablePresenter;
	private AreaTablePresenter areaTablePresenter;
	private ZoneTablePresenter zoneTablePresenter;
	private ZoneListPresenter zoneListPresenter;

	private PlayerPresenter playerPresenter;

	private IRpcServiceProvider rpcProvider;
	private Display display;
	
	public ContentPresenter(IRpcServiceProvider rpcProvider, HandlerManager eBus, Display view) {
		super(eBus);
		this.display = view;
		this.rpcProvider = rpcProvider;
	}

	@Override
	public void go() {
	}

	public void showDevice() {
		String id = AppToken.getDeviceId();
		if (!ClientUtil.isLongNumber(id)) {
			final String token = AppToken.newDevicesToken();
			eventBus.fireEvent(new ChangeTokenEvent(token));
		}
		else {
			final Long deviceId = Long.parseLong(id);

			rpcProvider.getDeviceService().findById(deviceId, new MethodCallback<DeviceDto>() {
				@Override
				public void onFailure(Method method, Throwable throwable) {
					ClientUtil.handleRpcFailure(method, throwable, eventBus);
				}

				@Override
				public void onSuccess(Method method, DeviceDto response) {
					if (response instanceof MediaPlayerDto) {
						showPlayer(deviceId);
					}
				}
			});
		}
	}

	private void showPlayer(Long playerId) {
		// check device type here
		display.asPanel().addStyleName("player");
		getPlayerPresenter().setPlayerId(playerId);
		getPlayerPresenter().show(display.asPanel());
	}

	public void showDashboard() {
		display.asPanel().removeStyleName("player");
		getDashboardPresenter().show(display.asPanel());
	}
	
	public void showSceneTable() {
		display.asPanel().removeStyleName("player");
		getSceneTablePresenter().show(display.asPanel());
	}
	
	public void showSceneList() {
		display.asPanel().removeStyleName("player");
		getSceneListPresenter().show(display.asPanel());
	}
	
	public void showDeviceList() {
		display.asPanel().removeStyleName("player");	
		getDeviceListPresenter().show(display.asPanel());
	}
	
	public void showDeviceTable() {
		display.asPanel().removeStyleName("player");
		getDeviceTablePresenter().show(display.asPanel());
	}
	
	public void showModeTable() {
		display.asPanel().removeStyleName("player");
		getModeTablePresenter().show(display.asPanel());
	}
	
	public void showModeList() {
		display.asPanel().removeStyleName("player");
		getModeListPresenter().show(display.asPanel());
	}

	public void showScheduleTable() {
		display.asPanel().removeStyleName("player");
		getScheduleTablePresenter().show(display.asPanel());
	}
	
	public void showControllerTable() {
		display.asPanel().removeStyleName("player");
		getControllerTablePresenter().show(display.asPanel());
	}
	
	public void showRelayTable() {
		display.asPanel().removeStyleName("player");
		getRelayTablePresenter().show(display.asPanel());
	}
	
	public void showUserTable() {
		display.asPanel().removeStyleName("player");
		if (AppToken.hasTokenItem(AppToken.PROJECT)) {
			getUserTablePresenter().show(display.asPanel());
		}
		else {
			getSystemUserPresenter().show(display.asPanel());
		}
	}
	
	public void showAreaTable() {
		display.asPanel().removeStyleName("player");
		getAreaTablePresenter().show(display.asPanel());
	}
	
	public void showZoneTable() {
		display.asPanel().removeStyleName("player");
		getZoneTablePresenter().show(display.asPanel());
	}

	public void showZoneList() {
		display.asPanel().removeStyleName("player");	
		getZoneListPresenter().show(display.asPanel());
	}
	
	
	public void showSensorTable() {
		display.asPanel().removeStyleName("player");
		getSensorTablePresenter().show(display.asPanel());
	}
	
	public void showProjectTable() {
		display.asPanel().removeStyleName("player");
		getSystemProjectPresenter().show(display.asPanel());
	}
	
	public PlayerPresenter getPlayerPresenter() {
		if (playerPresenter == null) {
			playerPresenter = new PlayerPresenter(rpcProvider.getPlayerService(), eventBus, new PlayerView());
			playerPresenter.go();
		}
		return playerPresenter;
	}

	private ScheduleTablePresenter getScheduleTablePresenter() {
		if (scheduleTablePresenter == null) {
			scheduleTablePresenter = new ScheduleTablePresenter(rpcProvider, eventBus, new ScheduleTableView());
			scheduleTablePresenter.go();
		}
		else {
			scheduleTablePresenter.refresh();
		}
		return scheduleTablePresenter;
	}

	public SceneTablePresenter getSceneTablePresenter() {
		if (sceneTablePresenter == null) {
			sceneTablePresenter = new SceneTablePresenter(rpcProvider, eventBus, new SceneTableView());
			sceneTablePresenter.go();
		}
		else {
			sceneTablePresenter.refresh();
		}
		return sceneTablePresenter;
	}
	
	private SceneListPresenter getSceneListPresenter() {
		if (sceneListPresenter == null) {
			sceneListPresenter = new SceneListPresenter(rpcProvider, eventBus, new SceneListView());
			sceneListPresenter.go();
		}
		return sceneListPresenter;
	}
	

	public ModeTablePresenter getModeTablePresenter() {
		if (modeTablePresenter == null) {
			modeTablePresenter = new ModeTablePresenter(rpcProvider, eventBus, new ModeTableView());
			modeTablePresenter.go();
		}
		else {
			modeTablePresenter.refresh();
		}
		return modeTablePresenter;
	}
	
	public ModeListPresenter getModeListPresenter() {
		if (modeListPresenter == null) {
			modeListPresenter = new ModeListPresenter(rpcProvider, eventBus, new ModeListView());
			modeListPresenter.go();
		}
		return modeListPresenter;
	}
	
	public DashboardPresenter getDashboardPresenter() {
		if (dashboardPresenter == null) {
			dashboardPresenter = new DashboardPresenter(rpcProvider, eventBus, new DashboardView());
			dashboardPresenter.go();
		}
		return dashboardPresenter;
	}
	

	public ControllerTablePresenter getControllerTablePresenter() {
		if (controllerTablePresenter == null) {
			controllerTablePresenter = new ControllerTablePresenter(rpcProvider, eventBus, new ControllerTableView());
			controllerTablePresenter.go();
		}
		else {
			controllerTablePresenter.refresh();
		}
		return controllerTablePresenter;
	}
	
	public DeviceListPresenter getDeviceListPresenter() {
		if (deviceListPresenter == null) {
			deviceListPresenter = new DeviceListPresenter(rpcProvider, eventBus, new DeviceListView());
			deviceListPresenter.go();
		}
		return deviceListPresenter;
	}
	
	public DeviceTablePresenter getDeviceTablePresenter() {
		if (deviceTablePresenter == null) {
			deviceTablePresenter = new DeviceTablePresenter(rpcProvider, eventBus, new DeviceTableView());
			deviceTablePresenter.go();
		}
		return deviceTablePresenter;
	}
	
	public RelayTablePresenter getRelayTablePresenter() {
		if (relayTablePresenter == null) {
			relayTablePresenter = new RelayTablePresenter(rpcProvider, eventBus, new RelayTableView());
			relayTablePresenter.go();
		}
		else {
			relayTablePresenter.refresh();
		}
		return relayTablePresenter;
	}
	
	public UserTablePresenter getUserTablePresenter() {
		if (userTablePresenter == null) {
			userTablePresenter = new UserTablePresenter(rpcProvider, eventBus, new UserTableView());
			userTablePresenter.go();
		}
		else {
			userTablePresenter.refresh();
		}
		return userTablePresenter;
	}
	
	public SensorTablePresenter getSensorTablePresenter() {
		if (sensorTablePresenter == null) {
			sensorTablePresenter = new SensorTablePresenter(rpcProvider, eventBus, new SensorTableView());
			sensorTablePresenter.go();
		}
		else {
			sensorTablePresenter.refresh();
		}
		return sensorTablePresenter;
	}
	
	public AreaTablePresenter getAreaTablePresenter() {
		if (areaTablePresenter == null) {
			areaTablePresenter = new AreaTablePresenter(rpcProvider, eventBus, new AreaTableView());
			areaTablePresenter.go();
		}
		else {
			areaTablePresenter.refresh();
		}
		return areaTablePresenter;
	}
	

	public ZoneTablePresenter getZoneTablePresenter() {
		if (zoneTablePresenter == null) {
			zoneTablePresenter = new ZoneTablePresenter(rpcProvider, eventBus, new ZoneTableView());
			zoneTablePresenter.go();
		}
		else {
			zoneTablePresenter.refresh();
		}
		return zoneTablePresenter;
	}
	
	public ZoneListPresenter getZoneListPresenter() {
		if (zoneListPresenter == null) {
			zoneListPresenter = new ZoneListPresenter(rpcProvider, eventBus, new ZoneListView());
			zoneListPresenter.go();
		}
		return zoneListPresenter;
	}
	
	public ProjectTablePresenter getSystemProjectPresenter() {
		if (systemProjectPresenter == null) {
			systemProjectPresenter = new ProjectTablePresenter(rpcProvider, eventBus, new ProjectTableView());
			systemProjectPresenter.go();
		}
		else {
			systemProjectPresenter.refresh();
		}
		return systemProjectPresenter;
	}
	
	public SystemUsersPresenter getSystemUserPresenter() {
		if (systemUserPresenter == null) {
			systemUserPresenter = new SystemUsersPresenter(rpcProvider, eventBus, new UserTableView());
			systemUserPresenter.go();
		}
		else {
			systemUserPresenter.refresh();
		}
		return systemUserPresenter;
	}
	
	@Override
	public void show(HasWidgets container) {
		container.add(display.asPanel());
	}

}