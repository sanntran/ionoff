package net.ionoff.center.client.dashboard;


import java.util.ArrayList;
import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Panel;

import gwt.material.design.client.ui.MaterialCard;
import gwt.material.design.client.ui.MaterialLabel;
import net.ionoff.center.client.common.AbstractPresenter;
import net.ionoff.center.client.device.ApplianceCardView;
import net.ionoff.center.client.device.ApplianceCollapsibleView;
import net.ionoff.center.client.device.AppliancePresenter;
import net.ionoff.center.client.device.ApplianceView;
import net.ionoff.center.client.device.DevicePresenter;
import net.ionoff.center.client.device.LightPresenter;
import net.ionoff.center.client.device.LightView;
import net.ionoff.center.client.device.PlayerPresenter;
import net.ionoff.center.client.device.PlayerView;
import net.ionoff.center.client.device.WeighScalePresenter;
import net.ionoff.center.client.device.WeighScaleView;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.ApplianceDto;
import net.ionoff.center.shared.dto.DashboardDto;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.LightDto;
import net.ionoff.center.shared.dto.PlayerDto;
import net.ionoff.center.shared.dto.WeighScaleDto;

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
				String token = AppToken.newDeviceToken();
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
	
	protected void scheduleRefreshDashboard() {
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
			if (device instanceof LightDto) {
				showLight((LightDto) device);
			}
			else if (device instanceof PlayerDto) {
				showPlayer((PlayerDto) device);
			}
			else if (device instanceof WeighScaleDto) {
				showScale((WeighScaleDto) device);
			}
			else if (device instanceof ApplianceDto) {
				showAppliance((ApplianceDto) device);
			}
		}
	}
	
	private void showScale(WeighScaleDto scale) {
		WeighScaleView scaleView = new WeighScaleView();
		WeighScalePresenter scalePresenter = new WeighScalePresenter(rpcService, eventBus, scaleView, scale);
		scalePresenter.go();
		devicePresenters.add(scalePresenter);
		scalePresenter.show(display.getDeviceWrapper());
	}
	
	private void showPlayer(PlayerDto player) {
		PlayerView playerView = new PlayerView();
		PlayerPresenter playerPresenter = new PlayerPresenter(rpcService, eventBus, playerView, player);
		playerPresenter.go();
		devicePresenters.add(playerPresenter);
		playerPresenter.show(display.getDeviceWrapper());
	}

	private void showAppliance(ApplianceDto appliance) {
		ApplianceView applianceView = createApplianceView(appliance);
		AppliancePresenter appliancePresenter = new AppliancePresenter(rpcService, eventBus, applianceView, appliance);
		appliancePresenter.go();
		devicePresenters.add(appliancePresenter);
		appliancePresenter.show(display.getDeviceWrapper());
	}

	private ApplianceView createApplianceView(ApplianceDto appliance) {
		if (appliance.getRelays().size() > 1) {
			return new ApplianceCollapsibleView();
		}
		return new ApplianceCardView();
	}

	private void showLight(LightDto light) {
		LightView lightView = new LightView();
		LightPresenter lightPresenter = new LightPresenter(rpcService, eventBus, lightView, light);
		lightPresenter.go();
		devicePresenters.add(lightPresenter);
		lightPresenter.show(display.getDeviceWrapper());
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

	protected void updateDashboard(DashboardDto dashboard) {
		if (!isVisible()) {
			return;
		}
		// Device
		display.getLblDeviceOn().setText(dashboard.getDeviceOnCount() + " On");
		display.getLblDeviceOff().setText(dashboard.getDeviceOffCount() + " Off");
		display.getLblTotalDevice().setText(dashboard.getTotalDeviceCount() + "");
		// Controller
		display.getLblControllerOnline().setText(dashboard.getOnlineControllerCount() + " Online");
		display.getLblControllerOffline().setText(dashboard.getOfflineControllerCount() + " Offline");
		display.getLblTotalController().setText(dashboard.getTotalControllerCount() + "");
		
		// Mode
		display.getLblTotalMode().setText(dashboard.getTotalModeCount() + "");
		display.getLblActivatedMode().setText(dashboard.getActivatedModeName());
		
		// Schedule
		display.getLblTotalSchedule().setText(dashboard.getTotalScheduleCount() + "");
		if (dashboard.getNextSchedule() != null) {
			display.getLblNextSchedule().setText(dashboard.getNextSchedule() + ": " + dashboard.getNextScheduleTime());
		}
		else {
			display.getLblNextSchedule().setText("");
		}
		// Scene
		display.getLblTotalScene().setText(dashboard.getTotalSceneCount() + "");
		display.getLblLastTriggeredScene().setText("");
		
		// Server
		display.getServerChart().setValue(dashboard.getMemoryUsedPercent(),  dashboard.getDiskSpaceUsedPercent());
		
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
