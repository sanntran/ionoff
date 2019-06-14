package net.ionoff.center.client.dashboard;


import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Panel;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialRow;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.device.*;
import net.ionoff.center.client.event.ChangeProjectEvent;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.event.ChangeZoneEvent;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.storage.StorageService;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.client.zone.ZonePresenter;
import net.ionoff.center.client.zone.ZoneView;
import net.ionoff.center.shared.dto.*;
import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import java.util.ArrayList;
import java.util.List;

public class DashboardPresenter extends AbstractPresenter {

	public interface Display {
		Panel asPanel();
		MaterialRow getWrapper();
		MaterialLink getLblTitle();
	}
	
	private IRpcServiceProvider rpcService;
	private Timer timer;
	private Display display;
	private final List<DeviceSlicePresenter> devicePresenters;
	private final List<ZonePresenter> zonePresenters;
	private int updatingDashboard = 0;
	
	public DashboardPresenter(IRpcServiceProvider rpcService, HandlerManager eventBus, Display view) {		
		super(eventBus);
		this.rpcService = rpcService;
		this.display = view;
		devicePresenters = new ArrayList<>();
		zonePresenters = new ArrayList<>();
	}
	
	private void bind() {
		if (!AppToken.hasTokenItem(AppToken.ZONE)) {
			Long projectId = AppToken.getProjectIdLong();
			UserDto user = StorageService.getInstance().getCookie().getUser();
			for (ProjectDto project : user.getProjects()) {
				if (projectId != null && projectId.equals(project.getId())) {
					display.getLblTitle().setText(project.getName());
					break;
				}
			}
		} else {
			Long zoneId = AppToken.getZoneIdLong();
			Long projectId = AppToken.getProjectIdLong();
			UserDto user = StorageService.getInstance().getCookie().getUser();
			for (ProjectDto project : user.getProjects()) {
				if (projectId != null && projectId.equals(project.getId())) {
					for (ZoneDto zone : project.getZones()) {
						if (zoneId != null && zoneId.equals(zone.getId())) {
							display.getLblTitle().setText(zone.getName());
							break;
						}
					}
					break;
				}
			}
		}
		eventBus.addHandler(ChangeProjectEvent.TYPE, event -> display.getLblTitle().setText(event.getProject().getName()));
		eventBus.addHandler(ChangeZoneEvent.TYPE, event -> display.getLblTitle().setText(event.getZone().getName()));
	}

	private void scheduleRefreshDashboard() {
		if (timer == null) {
			timer = new Timer() {
				@Override
				public void run() {
					if (!isVisible()) {
						timer.cancel();
					}
					else {
						refreshDashboard();
					}
				}
			};
		}
		timer.scheduleRepeating(5000);
	}

	private void getAndShowDashboard() {

		if (AppToken.hasTokenItem(AppToken.ZONE)) {
			rpcService.getDashboardService().findByZoneId(AppToken.getZoneIdLong(), 
					new MethodCallback<DashboardDto>() {
				@Override
				public void onFailure(Method method, Throwable exception) {
					ClientUtil.handleRpcFailure(method, exception, eventBus);
				}
				@Override
				public void onSuccess(Method method, DashboardDto response) {
					eventBus.fireEvent(ShowLoadingEvent.getInstance(false));				
					updateDashboard(response);
					showDevices(response.getDevices());
				}
			});
		}
		else {
			Long projectId = AppToken.getProjectIdLong();
			UserDto user = StorageService.getInstance().getCookie().getUser();
			for (ProjectDto project : user.getProjects()) {
				if (projectId != null && projectId.equals(project.getId())) {
					if (project.getZones().size() == 1) {
						String token = AppToken.newZoneDashboardToken(project.getZones().get(0).getId());
						eventBus.fireEvent(new ChangeTokenEvent(token));
						eventBus.fireEvent(new ChangeZoneEvent(project.getZones().get(0)));
						return;
					}
					break;
				}
			}
			rpcService.getDashboardService().findByProjectId(AppToken.getProjectIdLong(), 
					new MethodCallback<DashboardDto>() {
				@Override
				public void onFailure(Method method, Throwable exception) {
					ClientUtil.handleRpcFailure(method, exception, eventBus);
				}
				@Override
				public void onSuccess(Method method, DashboardDto response) {
					eventBus.fireEvent(ShowLoadingEvent.getInstance(false));				
					updateDashboard(response);
					showZones(response.getZones());
				}
			});
		}

	}
	private void showZones(List<ZoneDto> zones) {
		for (final ZoneDto zone : zones) {
			ZoneView sceneView = new ZoneView();
			ZonePresenter zonePresenter = new ZonePresenter(rpcService, eventBus, zone, sceneView);
			zonePresenter.go();
			zonePresenters.add(zonePresenter);
			zonePresenter.show(display.getWrapper());
		}
	}

	
	private void showDevices(List<DeviceDto> devices) {

		for (final DeviceDto device : devices) {
			if (device instanceof MediaPlayerDto) {
				showMediaPlayer((MediaPlayerDto) device);
			}
			else if (device instanceof RelayLoadDto) {
				showRelayLoad((RelayLoadDto) device);
			}
		}
	}

	private void showMediaPlayer(MediaPlayerDto player) {
		PlayerSliceView playerView = new PlayerSliceView();
		PlayerSlicePresenter playerPresenter = new PlayerSlicePresenter(rpcService, eventBus, playerView, player);
		playerPresenter.go();
		devicePresenters.add(playerPresenter);
		playerPresenter.show(display.getWrapper());
	}

	private void showRelayLoad(RelayLoadDto relayLoad) {
		RelayLoadSliceView relayLoadView = new RelayLoadSliceView();
		RelayLoadSlicePresenter relayLoadPresenter = new RelayLoadSlicePresenter(rpcService, eventBus, relayLoadView, relayLoad);
		relayLoadPresenter.go();
		devicePresenters.add(relayLoadPresenter);
		relayLoadPresenter.show(display.getWrapper());
	}

	private void refreshDashboard() {
		if (updatingDashboard > 0) {
			return;
		}
		updatingDashboard = updatingDashboard + 1;
		if (AppToken.hasTokenItem(AppToken.ZONE)) {
			rpcService.getDashboardService().findByZoneId(AppToken.getZoneIdLong(), 
					new MethodCallback<DashboardDto>() {
				@Override
				public void onFailure(Method method, Throwable exception) {
					ClientUtil.handleRpcFailure(method, exception, eventBus);
				}
				@Override
				public void onSuccess(Method method, DashboardDto response) {
					eventBus.fireEvent(ShowLoadingEvent.getInstance(false));				
					updateDashboard(response);
				}
			});
		}
		else {
			rpcService.getDashboardService().findByProjectId(AppToken.getProjectIdLong(), 
					new MethodCallback<DashboardDto>() {
				@Override
				public void onFailure(Method method, Throwable exception) {
					ClientUtil.handleRpcFailure(method, exception, eventBus);
				}
				@Override
				public void onSuccess(Method method, DashboardDto response) {
					eventBus.fireEvent(ShowLoadingEvent.getInstance(false));				
					updateDashboard(response);
				}
			});
		}
	}

	private void updateDashboard(DashboardDto dashboard) {
		if (!isVisible()) {
			return;
		}
		updatingDashboard = updatingDashboard - 1;
	}

	private void updateDeviceStatus(DeviceSlicePresenter devicePresenter, List<DeviceDto> devices) {
		if (devicePresenter.isLocked()) {
			return;
		}
		for (final DeviceDto device : devices) {
			if (device.getId() == devicePresenter.getDevice().getId()) {
				devicePresenter.updateStatus(device.getStatus());
				break;
			}
		}
	}

	protected boolean isVisible() {
		return AppToken.hasTokenItem(AppToken.DASHBOARD);
	}
	
	@Override
	public void go() {
		bind();
	}
	
	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(display.asPanel());
		display.getWrapper().clear();
		getAndShowDashboard();
		scheduleRefreshDashboard();
	}
}
