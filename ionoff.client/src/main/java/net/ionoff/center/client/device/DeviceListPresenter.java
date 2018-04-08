package net.ionoff.center.client.device;


import java.util.ArrayList;
import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Panel;

import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialRow;
import net.ionoff.center.client.common.AbstractPresenter;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.ApplianceDto;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.LightDto;
import net.ionoff.center.shared.dto.PlayerDto;
import net.ionoff.center.shared.dto.StatusDto;
import net.ionoff.center.shared.dto.WeighScaleDto;

public class DeviceListPresenter extends AbstractPresenter {

	public interface Display {
		Panel asPanel();
		MaterialRow getWrapper();
		MaterialIcon getIconSetting();
	}
	
	int page;
	private Timer timer;
	private Display display;
	private IRpcServiceProvider rpcProvider;
	private final List<DevicePresenter> devicePresenters;
	
	public DeviceListPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus, Display view) {		
		super(eventBus);
		this.display = view;
		this.rpcProvider = rpcProvider;
		devicePresenters = new ArrayList<>();
	}
	
	private void bind() {
		timer = new Timer() {
			@Override
			public void run() {
				if (!isVisible()) {
					timer.cancel();
				}
				else {
					rpcSyncStatusByDeviceIds();
				}
			}
		};
		display.getIconSetting().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				String token = AppToken.newDeviceTableToken();
				eventBus.fireEvent(new ChangeTokenEvent(token));
			}
		});
	}
	
	private void scheduleSyncDeviceStatus() {
		timer.scheduleRepeating(5000);
	}
	
	private void rpcSyncStatusByDeviceIds() {
		if (AppToken.hasTokenItem(AppToken.ZONE)) {
			rpcProvider.getDeviceService().getStatusByZoneId(AppToken.getZoneIdLong(), new MethodCallback<List<StatusDto>>() {
				@Override
				public void onFailure(Method method, Throwable exception) {
					ClientUtil.handleRpcFailure(method, exception, eventBus);
				}
				@Override
				public void onSuccess(Method method, List<StatusDto> response) {
					eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
					updateDevicesStatus(response);
				}
			});
		}
		else {
			rpcProvider.getDeviceService().getStatusByProjectId(AppToken.getProjectIdLong(), 
					new MethodCallback<List<StatusDto>>() {
				@Override
				public void onFailure(Method method, Throwable exception) {
					ClientUtil.handleRpcFailure(method, exception, eventBus);
				}
				@Override
				public void onSuccess(Method method, List<StatusDto> response) {
					eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
					updateDevicesStatus(response);
				}
			});
		}
		
	}
	
	private void updateDevicesStatus(List<StatusDto> response) {
		for (final DevicePresenter devicePresenter : devicePresenters) {
			updateDeviceStatus(devicePresenter, response);
		}
	}

	private void updateDeviceStatus(DevicePresenter devicePresenter, List<StatusDto> statusDtos) {
		if (devicePresenter.isLocked()) {
			return;
		}
		for (final StatusDto status : statusDtos) {
			if (status.getId() == devicePresenter.getDevice().getId()) {
				devicePresenter.updateStatus(status);
			}
		}
	}

	private void rpcLoadDevicesbyUser() {
		if (AppToken.hasTokenItem(AppToken.ZONE)) {
			rpcGetDevicesByZone(AppToken.getZoneIdLong());
		}
		else {
			rpcGetDevicesByProject(AppToken.getProjectIdLong());
		}
	}
	
	private void rpcGetDevicesByProject(long projectId) {
		rpcProvider.getDeviceService().findByProjectId(projectId, new MethodCallback<List<DeviceDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<DeviceDto> response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				showDevices(response);
			}
		});
	}

	private void rpcGetDevicesByZone(long zoneId) {
		rpcProvider.getDeviceService().findByZoneId(zoneId,
				new MethodCallback<List<DeviceDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, List<DeviceDto> response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				showDevices(response);
			}
		});
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
		scheduleSyncDeviceStatus();
	}
	
	private void showScale(WeighScaleDto scale) {
		WeighScaleView scaleView = new WeighScaleView();
		WeighScalePresenter scalePresenter = new WeighScalePresenter(rpcProvider, eventBus, scaleView, scale);
		scalePresenter.go();
		devicePresenters.add(scalePresenter);
		scalePresenter.show(display.getWrapper());
	}

	private void showPlayer(PlayerDto player) {
		PlayerView playerView = new PlayerView();
		PlayerPresenter playerPresenter = new PlayerPresenter(rpcProvider, eventBus, playerView, player);
		playerPresenter.go();
		devicePresenters.add(playerPresenter);
		playerPresenter.show(display.getWrapper());
	}

	private void showAppliance(ApplianceDto appliance) {
		ApplianceView applianceView = createApplianceView(appliance);
		AppliancePresenter appliancePresenter = new AppliancePresenter(rpcProvider, eventBus, applianceView, appliance);
		appliancePresenter.go();
		devicePresenters.add(appliancePresenter);
		appliancePresenter.show(display.getWrapper());
	}

	private ApplianceView createApplianceView(ApplianceDto appliance) {
		if (appliance.getRelays().size() > 1) {
			return new ApplianceCollapsibleView();
		}
		return new ApplianceCardView();
	}

	private void showLight(LightDto light) {
		LightView lightView = new LightView();
		LightPresenter lightPresenter = new LightPresenter(rpcProvider, eventBus, lightView, light);
		lightPresenter.go();
		devicePresenters.add(lightPresenter);
		lightPresenter.show(display.getWrapper());
	}

	protected boolean isVisible() {
		return AppToken.hasTokenItem(AppToken.DEVICES) && !AppToken.hasTokenItem(AppToken.TABLE);
	}
	
	@Override
	public void go() {
		bind();
	}
	
	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(display.asPanel());
		devicePresenters.clear();
		display.getWrapper().clear();
		rpcLoadDevicesbyUser();
	}
}
