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
import net.ionoff.center.shared.dto.ApplianceDto;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.StatusDto;

public class AppliancePresenter extends DevicePresenter {

	private ApplianceDto appliance;
	private final ApplianceView view;

	public AppliancePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus, ApplianceView view, ApplianceDto appliance) {
		super(rpcProvider, eventBus, appliance);
		this.view = view;
		this.appliance = appliance;
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
		view.setMenuDropdownId(appliance.getId());
		view.getLblName().setText(appliance.getName());
		view.getLblZone().setText(getDevice().getZoneName());
		
		displayStatus();
		
		view.getMenuItemAddToZoneDashboard().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				getRpcProvider().getDeviceService().addToZoneDashboard(
						appliance.getId(), appliance.getZoneId(), new MethodCallback<DeviceDto>() {
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
						appliance.getId(), appliance.getProjectId(), new MethodCallback<MessageDto>() {
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
							appliance.getId(), appliance.getZoneId(), new MethodCallback<MessageDto>() {
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
							appliance.getId(), appliance.getProjectId(), new MethodCallback<MessageDto>() {
						@Override
						public void onFailure(Method method, Throwable exception) {
							ClientUtil.handleRpcFailure(method, exception, eventBus);
						}
						@Override
						public void onSuccess(Method method, MessageDto response) {
							eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
							view.asPanel().removeFromParent();
						}
					});
				}
			}
		});

		if (!appliance.hasRelay()) {
			return;
		}
		
		if (view instanceof ApplianceCardView) {
			RelayDto relay = appliance.getRelays().get(0);
			if (relay.isButton()) {
				view.getBtnSwitch().addStyleName("press");
			}
			view.getBtnSwitch().addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					view.getBtnSwitch().setEnabled(false);
					if (relay.isButton()) {
						pressBtn(relay);
					}
					else {
						doSwitch(relay);
					}
				}
			});
		}
		else if (view instanceof ApplianceCollapsibleView) {
			ApplianceCollapsibleView collapsibleView = (ApplianceCollapsibleView) view;
			for (RelayDto relay : appliance.getRelays()) {
				RelayView relayView = new RelayView(relay);
				collapsibleView.getRelayViews().add(relayView);
				relayView.getBtnSwitch().addClickHandler(new ClickHandler() {
					@Override
					public void onClick(ClickEvent event) {
						relayView.getBtnSwitch().setEnabled(false);
						if (relayView.getRelay().isButton()) {
							pressBtn(relayView);
						}
						else {
							doSwitch(relayView);
						}
					}
				});
				collapsibleView.getRelayCollection().add(relayView.asPanel());
			}
		}
		
	}

	/**
	 * This method apply only for card view
	 */
	private void openRelay(final RelayDto relay) {
		setLocked(true);
		getRpcProvider().getRelayService().openRelay(relay.getId(), new MethodCallback<StatusDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				setLocked(false);
				MessageDto message = ClientUtil.handleRpcFailure(method, exception, eventBus);
				if (MessageDto.RelayDriverConnectException.equals(message.getCode())) {
					view.getBtnSwitch().setValue(true);
				}
				else {
					relay.setStatus(false);
					appliance.getStatus().setValue(false);
					displayStatus();
				}
				view.getBtnSwitch().setEnabled(true);
			}
			@Override
			public void onSuccess(Method method, StatusDto response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				setLocked(false);
				relay.setStatus(response.getValue());
				relay.setTime(response.getTime());
				appliance.getStatus().setValue(response.getValue());
				appliance.getStatus().setTime(response.getTime());
				view.getBtnSwitch().setEnabled(true);
				displayStatus();
			}
		});
	}
	
	/**
	 * This method apply only for card view
	 */
	private void closeRelay(final RelayDto relay) {
		setLocked(true);
		getRpcProvider().getRelayService().closeRelay(relay.getId(), new MethodCallback<StatusDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				setLocked(false);
				MessageDto message = ClientUtil.handleRpcFailure(method, exception, eventBus);
				if (MessageDto.RelayDriverConnectException.equals(message.getCode())) {
					view.getBtnSwitch().setValue(false);
				}
				else {
					relay.setStatus(true);
					appliance.getStatus().setValue(true);
					displayStatus();
				}
				view.getBtnSwitch().setEnabled(true);
			}
			@Override
			public void onSuccess(Method method, StatusDto response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				setLocked(false);
				relay.setStatus(response.getValue());
				relay.setTime(response.getTime());
				appliance.getStatus().setValue(response.getValue());
				appliance.getStatus().setTime(response.getTime());
				view.getBtnSwitch().setEnabled(true);
				displayStatus();
			}
		});
	}
	
	
	/**
	 * This method apply only for card view
	 */
	private void pressBtn(final RelayDto relay) {
		setLocked(true);
		getRpcProvider().getRelayService().closeOpenRelay(relay.getId(), new MethodCallback<StatusDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				setLocked(false);
				ClientUtil.handleRpcFailure(method, exception, eventBus);
				relay.setStatus(false);
				appliance.getStatus().setValue(false);
				view.getBtnSwitch().setEnabled(true);
				displayStatus();
			}
			@Override
			public void onSuccess(Method method, StatusDto response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				setLocked(false);
				relay.setStatus(response.getValue());
				relay.setTime(response.getTime());
				appliance.getStatus().setValue(response.getValue());
				appliance.getStatus().setTime(response.getTime());
				view.getBtnSwitch().setEnabled(true);
				displayStatus();
			}
		});
	}
	
	/**
	 * This method apply only for card view
	 */
	private void doSwitch(RelayDto relay) {
		if (Boolean.FALSE.equals(relay.getStatus())) {
			closeRelay(relay);
		}
		else if (Boolean.TRUE.equals(relay.getStatus())) {
			openRelay(relay);
		}
	}

	private void pressBtn(final RelayView relayView) {
		setLocked(true);
		relayView.getRelay().setStatus(true);
		relayView.displayStatus();
		
		getRpcProvider().getRelayService().closeOpenRelay(relayView.getRelay().getId(), new MethodCallback<StatusDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				setLocked(false);
				ClientUtil.handleRpcFailure(method, exception, eventBus);
				relayView.getRelay().setStatus(false);
				relayView.displayStatus();
				relayView.getBtnSwitch().setEnabled(true);
				displayStatus();
			}
			@Override
			public void onSuccess(Method method, StatusDto response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				setLocked(false);
				relayView.getRelay().setStatus(false);
				relayView.displayStatus();
				relayView.getBtnSwitch().setEnabled(true);
				displayStatus();
			}
		});
	}
	
	private void doSwitch(RelayView relayView) {
		if (relayView.getRelay().getStatus() == null) {
			return;
		}
		if (relayView.getRelay().getStatus() == false) {
			closeRelay(relayView);
		}
		else {
			openRelay(relayView);
		}
	}

	private void openRelay(final RelayView relayView) {
		setLocked(true);
		getRpcProvider().getRelayService().openRelay(relayView.getRelay().getId(), new MethodCallback<StatusDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				setLocked(false);
				MessageDto message = ClientUtil.handleRpcFailure(method, exception, eventBus);
				if (MessageDto.RelayDriverConnectException.equals(message.getCode())) {
					relayView.getRelay().setStatus(true);
					relayView.displayStatus();
					relayView.getBtnSwitch().setEnabled(true);
				}
				else {
					relayView.getRelay().setStatus(false);
					relayView.displayStatus();
					relayView.getBtnSwitch().setEnabled(true);
				}
			}
			@Override
			public void onSuccess(Method method, StatusDto response) {
				setLocked(false);
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				relayView.getRelay().setStatus(false);
				relayView.displayStatus();
				relayView.getBtnSwitch().setEnabled(true);
			}
		});
	}

	private void closeRelay(final RelayView relayView) {
		setLocked(true);
		relayView.getBtnSwitch().setEnabled(false);
		getRpcProvider().getRelayService().closeRelay(relayView.getRelay().getId(), new MethodCallback<StatusDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				setLocked(false);
				MessageDto message = ClientUtil.handleRpcFailure(method, exception, eventBus);
				if (MessageDto.RelayDriverConnectException.equals(message.getCode())) {
					relayView.getBtnSwitch().setEnabled(true);
					relayView.getRelay().setStatus(false);
					relayView.displayStatus();
				}
				else {
					relayView.getBtnSwitch().setEnabled(true);
					relayView.getRelay().setStatus(true);
					relayView.displayStatus();
				}
			}
			@Override
			public void onSuccess(Method method, StatusDto response) {
				setLocked(false);
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				relayView.getBtnSwitch().setEnabled(true);
				relayView.getRelay().setStatus(true);
				relayView.getRelay().setTime(response.getTime());
				relayView.displayStatus();
			}
		});
	}


	private void displayStatus() {
		view.asPanel().removeStyleName("on");
		if (appliance.getStatus().getTime() != null) {
			view.getLblTime().setText(appliance.getStatus().getTime());
		}
		if (Boolean.FALSE.equals(appliance.getStatus().getValue())) {
			view.getImgIcon().removeStyleName("on");
			view.getImgIcon().removeStyleName("unknown");
			view.getImgIcon().addStyleName("off");
		}
		else if (Boolean.TRUE.equals(appliance.getStatus().getValue())) {
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
		view.getLblTime().setText(appliance.getStatus().getTime());
		if (view instanceof ApplianceCardView) {
			if (!appliance.hasRelay()) {
				view.getBtnSwitch().setEnabled(false);
				view.getBtnSwitch().setValue(false);
				return;
			}
			view.getBtnSwitch().setEnabled(true);
			if (Boolean.FALSE.equals(appliance.getStatus().getValue())) {
				view.getBtnSwitch().setValue(false);
				
			} else if (Boolean.TRUE.equals(appliance.getStatus().getValue())) {
				view.getBtnSwitch().setValue(true);
				
			} else {
				view.getBtnSwitch().setValue(false);
			}
		}
		else if (view instanceof ApplianceCollapsibleView) {
			for (RelayView relayView : view.getRelayViews()) {
				relayView.displayStatus();
			}
		}
	}
	
	@Override
	public void updateStatus(StatusDto status) {
		if (isLocked()) {
			return;
		}
		appliance.getStatus().setValue(status.getValue());
		appliance.getStatus().setTime(status.getTime());
		if (appliance.hasOneRelay()) {
			appliance.getRelays().get(0).setStatus(status.getValue());
			appliance.getRelays().get(0).setTime(status.getTime());
		}
		if (appliance.hasManyRelays()) {
			for (StatusDto child : status.getChildren()) {
				for (RelayView relayView : view.getRelayViews()) {
					if (child.getId().equals(relayView.getRelay().getId())) {
						relayView.setStatus(status);
					}
				}
			}
		}
		displayStatus();
	}

	@Override
	protected ApplianceView getDeviceView() {
		return view;
	}
}
