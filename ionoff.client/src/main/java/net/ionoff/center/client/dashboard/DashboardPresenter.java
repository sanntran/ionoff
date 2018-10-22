package net.ionoff.center.client.dashboard;


import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Panel;
import gwt.material.design.client.ui.MaterialCard;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialRow;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.device.DevicePresenter;
import net.ionoff.center.client.device.PlayerPresenter;
import net.ionoff.center.client.device.PlayerView;
import net.ionoff.center.client.device.RelayLoadPresenter;
import net.ionoff.center.client.device.RelayLoadView;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.client.zone.ZonePresenter;
import net.ionoff.center.client.zone.ZoneView;
import net.ionoff.center.shared.dto.DashboardDto;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.MediaPlayerDto;
import net.ionoff.center.shared.dto.RelayLoadDto;
import net.ionoff.center.shared.dto.ZoneDto;
import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import java.util.ArrayList;
import java.util.List;

public class DashboardPresenter extends AbstractPresenter {

	public interface Display {
		Panel asPanel();
		MaterialRow getWrapper();
	}
	
	private IRpcServiceProvider rpcService;
	private Timer timer;
	private Display display;
	private final List<DevicePresenter> devicePresenters;
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
		PlayerView playerView = new PlayerView();
		PlayerPresenter playerPresenter = new PlayerPresenter(rpcService, eventBus, playerView, player);
		playerPresenter.go();
		devicePresenters.add(playerPresenter);
		playerPresenter.show(display.getWrapper());
	}

	private void showRelayLoad(RelayLoadDto relayLoad) {
		RelayLoadView relayLoadView = new RelayLoadView();
		RelayLoadPresenter relayLoadPresenter = new RelayLoadPresenter(rpcService, eventBus, relayLoadView, relayLoad);
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

	private void updateDeviceStatus(DevicePresenter devicePresenter, List<DeviceDto> devices) {
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
		if (AppToken.hasTokenItem(AppToken.ZONE)) {
		}
		else if (AppToken.hasTokenItem(AppToken.PROJECT)) {
		}
		getAndShowDashboard();
		scheduleRefreshDashboard();
	}
}
