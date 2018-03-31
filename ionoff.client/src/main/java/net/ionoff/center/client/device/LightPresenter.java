package net.ionoff.center.client.device;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.LightDto;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.StatusDto;

public class LightPresenter extends DevicePresenter {

	private LightDto light;
	private final LightView view;

	public LightPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus, LightView view, LightDto light) {
		super(rpcProvider, eventBus, light);
		this.view = view;
		this.light = light;
	}

	@Override
	public void show(HasWidgets container) {
		container.add(view.asPanel());
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
		view.setMenuDropdownId(light.getId());
		view.getLblName().setText(getDevice().getName());
		view.getLblZone().setText(getDevice().getZoneName());
		if (light.getStatus().getTime() != null) {
			view.getLblTime().setText(light.getStatus().getTime());
		}
		view.getBtnSwitch().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				doSwitch();
			}
		});
		view.getMenuItemAddToZoneDashboard().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				getRpcProvider().getDeviceService().addToZoneDashboard(
						light.getId(), light.getZoneId(), new MethodCallback<DeviceDto>() {
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
						light.getId(), light.getProjectId(), new MethodCallback<MessageDto>() {
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
							light.getId(), light.getZoneId(), new MethodCallback<MessageDto>() {
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
							light.getId(), light.getProjectId(), new MethodCallback<MessageDto>() {
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
		displayStatus();
	}

	private void doSwitch() {
		setLocked(true);
		view.getBtnSwitch().setEnabled(false);
		if (Boolean.FALSE.equals(light.getStatus().getValue())) {
			switchOn();
		}
		else if (Boolean.TRUE.equals(light.getStatus().getValue())) {
			switchOff();
		}
	}

	private void switchOff() {
		getRpcProvider().getDeviceService().turnOffDevice(light.getId(), new MethodCallback<StatusDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				setLocked(false);
				MessageDto message = ClientUtil.handleRpcFailure(method, exception, eventBus);
				if (MessageDto.ControllerConnectException.equals(message.getCode())) {
					view.getBtnSwitch().setValue(true);
				}
				else {
					light.getStatus().setValue(true);
					displayStatus();
				}
				view.getBtnSwitch().setEnabled(true);
			}
			@Override
			public void onSuccess(Method method, StatusDto response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				setLocked(false);
				updateStatus(response);
				view.getBtnSwitch().setEnabled(true);
			}
		});
	}

	private void switchOn() {
		setLocked(true);
		getRpcProvider().getDeviceService().turnOnDevice(light.getId(), new MethodCallback<StatusDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				setLocked(false);
				MessageDto message = ClientUtil.handleRpcFailure(method, exception, eventBus);
				if (MessageDto.ControllerConnectException.equals(message.getCode())) {
					view.getBtnSwitch().setValue(false);
				}
				else {
					light.getStatus().setValue(true);
					displayStatus();
				}
				view.getBtnSwitch().setEnabled(true);
			}
			@Override
			public void onSuccess(Method method, StatusDto response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				setLocked(false);
				updateStatus(response);
				view.getBtnSwitch().setEnabled(true);
			}
		});
	}


	private void displayStatus() {
		view.asPanel().removeStyleName("on");
		if (Boolean.FALSE.equals(light.getStatus().getValue())) {
			view.getImgIcon().removeStyleName("on");
			view.getImgIcon().removeStyleName("unknown");
			view.getImgIcon().addStyleName("off");
			view.getBtnSwitch().setEnabled(true);
			view.getBtnSwitch().setValue(false);
		}
		else if (Boolean.TRUE.equals(light.getStatus().getValue())) {
			view.getImgIcon().removeStyleName("off");
			view.getImgIcon().removeStyleName("unknown");
			view.getImgIcon().addStyleName("on");
			view.getBtnSwitch().setEnabled(true);
			view.getBtnSwitch().setValue(true);
			view.asPanel().addStyleName("on");
		}
		else { // status == null
			view.getImgIcon().removeStyleName("on");
			view.getImgIcon().removeStyleName("off");
			view.getImgIcon().addStyleName("unknown");
			view.getBtnSwitch().setValue(false);
			view.getBtnSwitch().setEnabled(false);
		}
		view.getLblTime().setText(light.getStatus().getTime());
	}

	@Override
	public void updateStatus(StatusDto status) {
		light.getStatus().setValue(status.getValue());
		light.getStatus().setTime(status.getTime());
		displayStatus();
	}

	@Override
	protected LightView getDeviceView() {
		return view;
	}
}
