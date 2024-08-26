package net.ionoff.center.client.dashboard;


import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Panel;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialRow;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.device.DeviceSlicePresenter;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.event.ChangeZoneEvent;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.service.RequestCallback;
import net.ionoff.center.client.storage.StorageService;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.*;
import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import java.util.ArrayList;
import java.util.List;

public class ProjectDashboardPresenter extends AbstractPresenter {

	public interface Display {
		Panel asPanel();
		MaterialRow getSliceRow();
		MaterialLink getLblTitle();
		void setPresenter(ProjectDashboardPresenter presenter);
		void updateCardDevice(DeviceStatisticDto deviceStatistic);
		void updateCardMode(ModeStatisticDto modeStatistic);
		void updateCardScene(SceneStatisticDto sceneStatistic);
		void updateCardSchedule(ScheduleStatisticDto scheduleStatistic);
		void updateCardController(ControllerStatisticDto controllerStatisticDto);
		void updateCardSensor(SensorStatisticDto sensorStatisticDto);
		void updateCardAlert(AlertStatisticDto alertStatistic);
		void updateCardArea(AreaStatisticDto areaStatistic);
		void updateCardZone(ZoneStatisticDto zoneStatistic);
	}
	
	private IRpcServiceProvider rpcService;
	private Timer timer;
	private Display display;
	private final List<DeviceSlicePresenter> devicePresenters;
	private int updatingDashboard = 0;
	
	public ProjectDashboardPresenter(IRpcServiceProvider rpcService, HandlerManager eventBus, Display view) {
		super(eventBus);
		this.rpcService = rpcService;
		this.display = view;
		this.devicePresenters = new ArrayList<>();
	}

	public void onCardAreaClick() {
		eventBus.fireEvent(new ChangeTokenEvent(AppToken.newAreaGridToken()));
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
				}
			});
		}
	}

	private void refreshDashboard() {
		if (updatingDashboard > 0) {
			return;
		}
		updatingDashboard = updatingDashboard + 1;
		if (AppToken.hasTokenItem(AppToken.ZONE)) {
			rpcService.getDashboardService().findByZoneId(
					AppToken.getZoneIdLong(), new RequestCallback<>(eventBus, (method, body) -> updateDashboard(body)));
		} else {
			rpcService.getDashboardService().findByProjectId(
					AppToken.getProjectIdLong(), new RequestCallback<>(eventBus, (method, body) -> updateDashboard(body)));
		}
	}

	private void updateDashboard(DashboardDto dashboard) {
		if (!isVisible()) {
			return;
		}
		display.updateCardDevice(dashboard.getDeviceStatistic());
		display.updateCardMode(dashboard.getModeStatistic());
		display.updateCardScene(dashboard.getSceneStatistic());
		display.updateCardSchedule(dashboard.getScheduleStatistic());
		display.updateCardController(dashboard.getControllerStatistic());
		display.updateCardSensor(dashboard.getSensorStatistic());
		display.updateCardAlert(dashboard.getAlertStatistic());
		display.updateCardArea(dashboard.getAreaStatistic());
		display.updateCardZone(dashboard.getZoneStatistic());
		updatingDashboard = updatingDashboard - 1;
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
		display.getSliceRow().clear();
		getAndShowDashboard();
		scheduleRefreshDashboard();
	}
}
