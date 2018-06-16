package net.ionoff.center.client.device;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.SensorDriverDto;
import net.ionoff.center.shared.dto.StatusDto;

public class SensorDriverPresenter extends DevicePresenter {

	private SensorDriverDto sensorDriver;
	private final SensorDriverView view;
	private final IRpcServiceProvider rpcService;
 
	public SensorDriverPresenter(IRpcServiceProvider rpcService, HandlerManager eventBus,
			SensorDriverView view, SensorDriverDto sensorDriver) {
		super(rpcService, eventBus, sensorDriver);
		this.view = view;
		this.sensorDriver = sensorDriver;
		this.rpcService = rpcService;
	}

	@Override
	public void show(HasWidgets container) {
		updateMenuItems();
		container.add(view.asPanel());
	}

	private void updateMenuItems() {
		if (AppToken.hasTokenItem(AppToken.DASHBOARD)) {
			view.getMenuItemAddToProjectDashboard().getParent().setVisible(false);
			view.getMenuItemAddToZoneDashboard().getParent().setVisible(false);
			view.getMenuItemRemoveFromDashboard().getParent().setVisible(true);
		}
		else {
			view.getMenuItemAddToProjectDashboard().getParent().setVisible(true);
			view.getMenuItemAddToZoneDashboard().getParent().setVisible(true);
			view.getMenuItemRemoveFromDashboard().getParent().setVisible(false);
		}
	}

	@Override
	public void bind() {
		view.setMenuDropdownId(sensorDriver.getId());
		view.getLblName().setText(getDevice().getName());
		view.getLblZone().setText(getDevice().getZoneName());
		displayStatus();
		view.getSensorDriverCard().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				String token = AppToken.newDeviceToken(sensorDriver.getId());
				eventBus.fireEvent(new ChangeTokenEvent(token));
			}
		});
		
		view.getMenuItemAddToZoneDashboard().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				getRpcProvider().getDeviceService().addToZoneDashboard(
						sensorDriver.getId(), sensorDriver.getZoneId(), new MethodCallback<DeviceDto>() {
					@Override
					public void onFailure(Method method, Throwable exception) {
						ClientUtil.handleRpcFailure(method, exception, eventBus);
					}
					@Override
					public void onSuccess(Method method, DeviceDto response) {
						eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
								ShowMessageEvent.SUCCESS));
					}
				});
			}
		});

		view.getMenuItemAddToProjectDashboard().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				getRpcProvider().getDeviceService().addToProjectDashboard(
						sensorDriver.getId(), sensorDriver.getProjectId(), new MethodCallback<MessageDto>() {
					@Override
					public void onFailure(Method method, Throwable exception) {
						ClientUtil.handleRpcFailure(method, exception, eventBus);
					}
					@Override
					public void onSuccess(Method method, MessageDto response) {
						eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
								ShowMessageEvent.SUCCESS));
					}
				});
			}
		});
		
		view.getMenuItemRemoveFromDashboard().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				
				if (AppToken.hasTokenItem(AppToken.ZONE)) {
					getRpcProvider().getDeviceService().removeFromZoneDashboard(
							sensorDriver.getId(), sensorDriver.getZoneId(), new MethodCallback<MessageDto>() {
						@Override
						public void onFailure(Method method, Throwable exception) {
							ClientUtil.handleRpcFailure(method, exception, eventBus);
						}
						@Override
						public void onSuccess(Method method, MessageDto response) {
							eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
									ShowMessageEvent.SUCCESS));
							view.asPanel().removeFromParent();
						}
					});
				}
				else {
					getRpcProvider().getDeviceService().removeFromProjectDashboard(
							sensorDriver.getId(), sensorDriver.getProjectId(), new MethodCallback<MessageDto>() {
						@Override
						public void onFailure(Method method, Throwable exception) {
							ClientUtil.handleRpcFailure(method, exception, eventBus);
						}
						@Override
						public void onSuccess(Method method, MessageDto response) {
							eventBus.fireEvent(new ShowMessageEvent(AdminLocale.getAdminMessages().updateSuccess(),
									ShowMessageEvent.SUCCESS));
							view.asPanel().removeFromParent();
						}
					});
				}
			}
		});
	}

	private void displayStatus() {
		view.asPanel().removeStyleName("on");
		
		if (sensorDriver.getStatus().getTime() != null) {
			view.getLblTime().setText(sensorDriver.getStatus().getTime());
		}
		view.getLblLatestValue().setText(sensorDriver.getStatus().getLatestValue());
		
		if (Boolean.FALSE.equals(sensorDriver.getStatus().getValue())) {
			view.getImgIcon().removeStyleName("on");
			view.getImgIcon().removeStyleName("unknown");
			view.getImgIcon().addStyleName("off");
		}
		else if (Boolean.TRUE.equals(sensorDriver.getStatus().getValue())) {
			view.getImgIcon().removeStyleName("off");
			view.getImgIcon().removeStyleName("unknown");
			view.getImgIcon().addStyleName("on");
			view.asPanel().addStyleName("on");
		}
		else { // status == null
			view.getImgIcon().removeStyleName("on");
			view.getImgIcon().removeStyleName("off");
			view.getImgIcon().addStyleName("unknown");
		}
	}

	@Override
	public void updateStatus(StatusDto status) {
		sensorDriver.getStatus().setValue(status.getValue());
		sensorDriver.getStatus().setLatestValue(status.getLatestValue());
		sensorDriver.getStatus().setTime(status.getTime());
		displayStatus();
	}

	@Override
	protected SensorDriverView getDeviceView() {
		return view;
	}
}