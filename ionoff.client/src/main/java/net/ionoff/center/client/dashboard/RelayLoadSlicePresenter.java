package net.ionoff.center.client.dashboard;

import gwt.material.design.client.constants.Color;
import net.ionoff.center.client.device.RelayView;
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
import net.ionoff.center.shared.dto.RelayLoadDto;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.RelayDto;
import net.ionoff.center.shared.dto.StatusDto;

public class RelayLoadSlicePresenter extends DeviceSlicePresenter {

	private RelayLoadDto relayLoad;
	private final RelayLoadSliceView view;

	public RelayLoadSlicePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus, RelayLoadSliceView view, RelayLoadDto appliance) {
		super(rpcProvider, eventBus, appliance);
		this.view = view;
		this.relayLoad = appliance;
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
		view.setMenuDropdownId(relayLoad.getId());
		view.getLblName().setText(relayLoad.getName());
		view.getLblZone().setText(getDevice().getZoneName());

		if (relayLoad.hasOneRelay()) {
			view.getLblRelay().setText(relayLoad.getRelays().get(0).getName());
			view.getLblController().setText(relayLoad.getRelays().get(0).getDriverName());
		}
		
		displayStatus();
		
		view.getMenuItemAddToZoneDashboard().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				getRpcProvider().getDeviceService().addToZoneDashboard(
						relayLoad.getId(), relayLoad.getZoneId(), new MethodCallback<DeviceDto>() {
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
						relayLoad.getId(), relayLoad.getProjectId(), new MethodCallback<MessageDto>() {
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
							relayLoad.getId(), relayLoad.getZoneId(), new MethodCallback<MessageDto>() {
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
							relayLoad.getId(), relayLoad.getProjectId(), new MethodCallback<MessageDto>() {
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

		if (!relayLoad.hasRelay()) {
			return;
		}

		if (relayLoad.getRelays().size() == 1) {
			RelayDto relay = relayLoad.getRelays().get(0);
			if (relay.izAutoRevert()) {
				view.getBtnSwitch().addStyleName("press");
			}
			view.getBtnSwitch().addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					view.getBtnSwitch().setEnabled(false);
					doSwitch(relay);
				}
			});
		}
		else if (view instanceof RelayLoadSliceView) {
			RelayLoadSliceView collapsibleView = (RelayLoadSliceView) view;
			for (RelayDto relay : relayLoad.getRelays()) {
				RelayView relayView = new RelayView(relay);
				collapsibleView.getRelayViews().add(relayView);
				relayView.getBtnSwitch().addClickHandler(new ClickHandler() {
					@Override
					public void onClick(ClickEvent event) {
						doSwitch(relayView);
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
				if (MessageDto.ControllerConnectException.equals(message.getCode())) {
					view.getBtnSwitch().setIconColor(Color.RED_ACCENT_3);
				}
				else {
					relay.setStatus(false);
					relayLoad.getStatus().setValue(false);
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
				relayLoad.getStatus().setValue(response.getValue());
				relayLoad.getStatus().setTime(response.getTime());
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
				if (MessageDto.ControllerConnectException.equals(message.getCode())) {
					view.getBtnSwitch().setIconColor(Color.GREEN_ACCENT_2);
				}
				else {
					relay.setStatus(true);
					relayLoad.getStatus().setValue(true);
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
				relayLoad.getStatus().setValue(response.getValue());
				relayLoad.getStatus().setTime(response.getTime());
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
				if (MessageDto.ControllerConnectException.equals(message.getCode())) {
					relayView.getRelay().setStatus(true);
					relayView.displayStatus();
				}
				else {
					relayView.getRelay().setStatus(false);
					relayView.displayStatus();
				}
			}
			@Override
			public void onSuccess(Method method, StatusDto response) {
				setLocked(false);
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				relayView.getRelay().setStatus(response.getValue());
				relayView.displayStatus();
			}
		});
	}

	private void closeRelay(final RelayView relayView) {
		setLocked(true);
		getRpcProvider().getRelayService().closeRelay(relayView.getRelay().getId(), new MethodCallback<StatusDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				setLocked(false);
				MessageDto message = ClientUtil.handleRpcFailure(method, exception, eventBus);
				if (MessageDto.ControllerConnectException.equals(message.getCode())) {
					relayView.getRelay().setStatus(false);
					relayView.displayStatus();
				}
				else {
					relayView.getRelay().setStatus(true);
					relayView.displayStatus();
				}
			}
			@Override
			public void onSuccess(Method method, StatusDto response) {
				setLocked(false);
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				relayView.getRelay().setStatus(response.getValue());
				relayView.getRelay().setTime(response.getTime());
				relayView.displayStatus();
			}
		});
	}


	private void displayStatus() {
		if (relayLoad.getStatus().getTime() != null) {
			view.getLblTime().setText(relayLoad.getStatus().getTime());
		}
		view.getLblTime().setText(relayLoad.getStatus().getTime());
		if (relayLoad.hasOneRelay()) {
			if (Boolean.FALSE.equals(relayLoad.getStatus().getValue())) {
				setStatus(false);
				
			} else if (Boolean.TRUE.equals(relayLoad.getStatus().getValue())) {
				setStatus(true);
				
			} else {
				setStatus(false);
			}
		}
		for (RelayView relayView : view.getRelayViews()) {
			relayView.displayStatus();
		}
	}
	
	@Override
	public void updateStatus(StatusDto status) {
		if (isLocked()) {
			return;
		}
		relayLoad.getStatus().setValue(status.getValue());
		relayLoad.getStatus().setTime(status.getTime());
		if (relayLoad.hasOneRelay()) {
			relayLoad.getRelays().get(0).setStatus(status.getValue());
			relayLoad.getRelays().get(0).setTime(status.getTime());
		}
		if (relayLoad.hasManyRelays()) {
			for (StatusDto child : status.getChildren()) {
				for (RelayView relayView : view.getRelayViews()) {
					if (child.getId().equals(relayView.getRelay().getId())) {
						relayView.setStatus(child);
						break;
					}
				}
			}
		}
		displayStatus();
	}

	@Override
	protected RelayLoadSliceView getDeviceView() {
		return view;
	}
}
