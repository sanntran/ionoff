package net.ionoff.center.client.device;


import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Panel;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialRow;
import net.ionoff.center.client.base.AbstractPresenter;
import net.ionoff.center.client.dashboard.DeviceSlicePresenter;
import net.ionoff.center.client.dashboard.PlayerSlicePresenter;
import net.ionoff.center.client.dashboard.PlayerSliceView;
import net.ionoff.center.client.dashboard.RelayLoadSlicePresenter;
import net.ionoff.center.client.dashboard.RelayLoadSliceView;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.MediaPlayerDto;
import net.ionoff.center.shared.dto.RelayLoadDto;
import net.ionoff.center.shared.dto.StatusDto;
import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import java.util.ArrayList;
import java.util.List;

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
	private final List<DeviceSlicePresenter> devicePresenters;
	
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
		for (final DeviceSlicePresenter devicePresenter : devicePresenters) {
			updateDeviceStatus(devicePresenter, response);
		}
	}

	private void updateDeviceStatus(DeviceSlicePresenter devicePresenter, List<StatusDto> statusDtos) {
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
			if (device instanceof MediaPlayerDto) {
				showPlayer((MediaPlayerDto) device);
			}
			else if (device instanceof RelayLoadDto) {
				showRelayLoad((RelayLoadDto) device);
			}
		}
		scheduleSyncDeviceStatus();
	}

	private void showPlayer(MediaPlayerDto player) {
		PlayerSliceView playerView = new PlayerSliceView();
		PlayerSlicePresenter playerPresenter = new PlayerSlicePresenter(rpcProvider, eventBus, playerView, player);
		playerPresenter.go();
		devicePresenters.add(playerPresenter);
		playerPresenter.show(display.getWrapper());
	}

	private void showRelayLoad(RelayLoadDto appliance) {
		RelayLoadSliceView relayLoadView = new RelayLoadSliceView();
		RelayLoadSlicePresenter appliancePresenter = new RelayLoadSlicePresenter(rpcProvider, eventBus, relayLoadView, appliance);
		appliancePresenter.go();
		devicePresenters.add(appliancePresenter);
		appliancePresenter.show(display.getWrapper());
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
