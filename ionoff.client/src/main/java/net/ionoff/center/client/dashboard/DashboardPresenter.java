package net.ionoff.center.client.dashboard;


import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Panel;
import gwt.material.design.client.ui.MaterialLink;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.device.*;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.event.ChangeZoneEvent;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.storage.StorageService;
import net.ionoff.center.client.ui.DashboardCard;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.*;
import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import java.util.ArrayList;
import java.util.List;

public class DashboardPresenter extends AbstractPresenter {

	public interface Display {
		Panel asPanel();
		MaterialLink getLblTitle();
		DashboardCard getCartDevice();
		DashboardCard getCartController();
		FlowPanel getDeviceWrapper();
		void setPresenter(DashboardPresenter presenter);
		void displayCartDeviceData(DeviceStatisticDto deviceStatistic);
		void displayCartModeData(ModeStatisticDto modeStatistic);
		void displayCartSceneData(SceneStatisticDto sceneStatistic);
		void displayCartScheduleData(ScheduleStatisticDto scheduleStatistic);
	}
	
	private IRpcServiceProvider rpcService;
	private Timer timer;
	private Display display;
	private final List<DeviceSlicePresenter> devicePresenters;
	private int updatingDashboard = 0;
	
	public DashboardPresenter(IRpcServiceProvider rpcService, HandlerManager eventBus, Display view) {		
		super(eventBus);
		this.rpcService = rpcService;
		this.display = view;
		this.devicePresenters = new ArrayList<>();
	}

	public void onCardAreaClick() {
		eventBus.fireEvent(new ChangeTokenEvent(AppToken.newAreaTableToken()));
	}
	public void onCardZoneClick() {
		eventBus.fireEvent(new ChangeTokenEvent(AppToken.newZoneTableToken()));
	}
	public void onCardDeviceClick() {
		eventBus.fireEvent(new ChangeTokenEvent(AppToken.newDeviceTableToken()));
	}
	public void onCartControllerClick() {
		eventBus.fireEvent(new ChangeTokenEvent(AppToken.newControllerTableToken()));
	}
	public void onCartSensorClick() {
		eventBus.fireEvent(new ChangeTokenEvent(AppToken.newSensorTableToken()));
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
					display.getLblTitle().setText(project.getName());
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
					showDevices(response.getDevices());
				}
			});
		}

	}

	
	private void showDevices(List<DeviceDto> devices) {
		for (final DeviceDto device : devices) {
			if (device instanceof MediaPlayerDto) {
				showMediaPlayer((MediaPlayerDto) device);
			} else if (device instanceof RelayLoadDto) {
				showRelayLoad((RelayLoadDto) device);
			}
		}
	}

	private void showMediaPlayer(MediaPlayerDto player) {
		PlayerSliceView playerView = new PlayerSliceView();
		PlayerSlicePresenter playerPresenter = new PlayerSlicePresenter(rpcService, eventBus, playerView, player);
		playerPresenter.go();
		devicePresenters.add(playerPresenter);
		playerPresenter.show(display.getDeviceWrapper());
	}

	private void showRelayLoad(RelayLoadDto relayLoad) {
		RelayLoadSliceView relayLoadView = new RelayLoadSliceView();
		RelayLoadSlicePresenter relayLoadPresenter = new RelayLoadSlicePresenter(rpcService, eventBus, relayLoadView, relayLoad);
		relayLoadPresenter.go();
		devicePresenters.add(relayLoadPresenter);
		relayLoadPresenter.show(display.getDeviceWrapper());
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

		display.displayCartDeviceData(dashboard.getDeviceStatistic());
		display.displayCartModeData(dashboard.getModeStatistic());
		display.displayCartSceneData(dashboard.getSceneStatistic());
		display.displayCartScheduleData(dashboard.getScheduleStatistic());

//
//		// Server
//		if (dashboard.getServerStatistic() != null) {
//			display.getServerChart().setValue(dashboard.getServerStatistic().getMemoryUsedPercent(),
//					dashboard.getServerStatistic().getDiskSpaceUsedPercent());
//		}

		updatingDashboard = updatingDashboard - 1;
	}

	private void updateDeviceStatus(DeviceSlicePresenter devicePresenter, List<DeviceDto> devices) {
		if (devicePresenter.isLocked() || devices == null || devices.isEmpty()) {
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
		display.setPresenter(this);
	}
	
	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(display.asPanel());
		display.getDeviceWrapper().clear();
		if (AppToken.hasTokenItem(AppToken.ZONE)) {
			display.getCartController().setVisible(false);
//			display.getCartMode().setVisible(false);
//			display.getCartServerChart().setVisible(false);
//			display.getCartSchedule().setVisible(false);
		}
		else if (AppToken.hasTokenItem(AppToken.PROJECT)) {
			display.getCartController().setVisible(true);
//			display.getCartMode().setVisible(true);
//			display.getCartServerChart().setVisible(true);
//			display.getCartSchedule().setVisible(true);
		}
		getAndShowDashboard();
		scheduleRefreshDashboard();
	}
}
