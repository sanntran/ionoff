package net.ionoff.center.client.dashboard;


import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Panel;
import gwt.material.design.client.ui.MaterialCard;
import gwt.material.design.client.ui.MaterialLabel;
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
import net.ionoff.center.shared.dto.DashboardDto;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.MediaPlayerDto;
import net.ionoff.center.shared.dto.RelayLoadDto;
import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import java.util.ArrayList;
import java.util.List;

public class DashboardPresenter extends AbstractPresenter {

	public interface Display {
		
		Panel asPanel();
		
		MaterialLabel getLblDeviceOn();

		MaterialLabel getLblDeviceOff();

		MaterialLabel getLblTotalDevice();

		MaterialLabel getLblTotalScene();

		MaterialLabel getLblLastTriggeredScene();

		MaterialLabel getLblTotalSchedule();

		MaterialLabel getLblNextSchedule();

		MaterialLabel getLblTotalController();

		MaterialLabel getLblControllerOnline();

		MaterialLabel getLblControllerOffline();

		MaterialLabel getLblTotalMode();

		MaterialLabel getLblActivatedMode();

		DashboardChartView getServerChart();

		MaterialCard getCartDevice();

		MaterialCard getCartScene();

		MaterialCard getCartSchedule();

		MaterialCard getCartMode();

		MaterialCard getCartController();

		MaterialCard getCartServerChart();

		FlowPanel getDeviceWrapper();

	}
	
	private IRpcServiceProvider rpcService;
	private Timer timer;
	private Display display;
	private final List<DevicePresenter> devicePresenters;
	private int updatingDashboard = 0;
	
	public DashboardPresenter(IRpcServiceProvider rpcService, HandlerManager eventBus, Display view) {		
		super(eventBus);
		this.rpcService = rpcService;
		this.display = view;
		devicePresenters = new ArrayList<>();
	}
	
	private void bind() {
		display.getCartDevice().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				String token = AppToken.newDevicesToken();
				eventBus.fireEvent(new ChangeTokenEvent(token));
			}
		});
		display.getCartScene().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				String token = AppToken.newSceneToken();
				eventBus.fireEvent(new ChangeTokenEvent(token));
			}
		});
		display.getCartMode().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				String token = AppToken.newModeToken();
				eventBus.fireEvent(new ChangeTokenEvent(token));
			}
		});
		display.getCartSchedule().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				String token = AppToken.newScheduleToken();
				eventBus.fireEvent(new ChangeTokenEvent(token));
			}
		});
		display.getCartController().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				String token = AppToken.newControllerToken();
				eventBus.fireEvent(new ChangeTokenEvent(token));
			}
		});
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
					showDevices(response.getDevices());
				}
			});
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
		playerPresenter.show(display.getDeviceWrapper());
	}

	private void showRelayLoad(RelayLoadDto relayLoad) {
		RelayLoadView relayLoadView = new RelayLoadView();
		RelayLoadPresenter appliancePresenter = new RelayLoadPresenter(rpcService, eventBus, relayLoadView, relayLoad);
		appliancePresenter.go();
		devicePresenters.add(appliancePresenter);
		appliancePresenter.show(display.getDeviceWrapper());
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
		// Device
		if (dashboard.getDeviceStatistic() != null) {
			display.getLblDeviceOn().setText(dashboard.getDeviceStatistic().getOnCount() + " On");
			display.getLblDeviceOff().setText(dashboard.getDeviceStatistic().getOffCount() + " Off");
			display.getLblTotalDevice().setText(dashboard.getDeviceStatistic().getTotalCount() + "");
		}

		// Controller
		if (dashboard.getControllerStatisticDto() != null) {
			display.getLblControllerOnline().setText(dashboard.getControllerStatisticDto().getOnlineCount() + " Online");
			display.getLblControllerOffline().setText(dashboard.getControllerStatisticDto().getOfflineCount() + " Offline");
			display.getLblTotalController().setText(dashboard.getControllerStatisticDto().getTotalCount() + "");
		}

		
		// Mode
		if (dashboard.getModeStatistic() != null) {
			display.getLblTotalMode().setText(dashboard.getModeStatistic().getTotalCount() + "");
			display.getLblActivatedMode().setText(dashboard.getModeStatistic().getActivatedName());
		}

		// ScheduleDto
		if (dashboard.getScheduleStatistic() != null) {
			display.getLblTotalSchedule().setText(dashboard.getScheduleStatistic().getTotalCount() + "");
			if (dashboard.getScheduleStatistic().getNextScheduleName() != null) {
				display.getLblNextSchedule().setText(dashboard.getScheduleStatistic().getNextScheduleName()
						+ ": " + dashboard.getScheduleStatistic().getNextScheduleTime());
			}
			else {
				display.getLblNextSchedule().setText("");
			}
		}

		// Scene
		if (dashboard.getSceneStatistic() != null) {
			display.getLblTotalScene().setText(dashboard.getSceneStatistic().getTotalCount() + "");
			display.getLblLastTriggeredScene().setText("");
		}

		// Server
		if (dashboard.getServerStatistic() != null) {
			display.getServerChart().setValue(dashboard.getServerStatistic().getMemoryUsedPercent(),
					dashboard.getServerStatistic().getDiskSpaceUsedPercent());
		}

		for (final DevicePresenter devicePresenter : devicePresenters) {
			updateDeviceStatus(devicePresenter, dashboard.getDevices());
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
		display.getDeviceWrapper().clear();
		if (AppToken.hasTokenItem(AppToken.ZONE)) {
			display.getCartController().setVisible(false);
			display.getCartMode().setVisible(false);
			display.getCartServerChart().setVisible(false);
			display.getCartSchedule().setVisible(false);
		}
		else if (AppToken.hasTokenItem(AppToken.PROJECT)) {
			display.getCartController().setVisible(true);
			display.getCartMode().setVisible(true);
			display.getCartServerChart().setVisible(true);
			display.getCartSchedule().setVisible(true);
		}
		getAndShowDashboard();
		scheduleRefreshDashboard();
	}
}
