package net.ionoff.center.client.device;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.constants.Color;
import gwt.material.design.client.constants.IconType;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.MessageDto;
import net.ionoff.center.shared.dto.PlayerDto;
import net.ionoff.center.shared.dto.StatusDto;
import net.xapxinh.center.client.player.event.ShowLoadingEvent;
import net.xapxinh.center.shared.dto.Command;
import net.xapxinh.center.shared.dto.PlayerApi;
import net.xapxinh.center.shared.dto.State;
import net.xapxinh.center.shared.dto.Status;

public class PlayerPresenter extends DevicePresenter {

	private PlayerDto player;
	private final PlayerView view;
	private final IRpcServiceProvider rpcService;

	public PlayerPresenter(IRpcServiceProvider rpcService, HandlerManager eventBus, PlayerView view, PlayerDto player) {
		super(rpcService, eventBus, player);
		this.view = view;
		this.player = player;
		this.rpcService = rpcService;
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
		view.setMenuDropdownId(player.getId());
		view.getLblName().setText(getDevice().getName());
		view.getLblZone().setText(getDevice().getZoneName());
		displayStatus();
		view.getPlayerCard().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				String token = AppToken.newDeviceToken(player.getId());
				eventBus.fireEvent(new ChangeTokenEvent(token));
				
			}
		});
		view.getBtnStop().addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					rpcSendCommand(PlayerApi.stopPlaylist());
				}
			});
		view.getBtnPlay().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (State.playing.toString().equals(player.getStatus().getState())) {
					rpcSendCommand(PlayerApi.pausePlaylistLeaf());
				}
				else if (State.paused.toString().equals(player.getStatus().getState())) {
					rpcSendCommand(PlayerApi.resumePlaylistLeaf());
				}
				else {
					rpcSendCommand(PlayerApi.playPlaylist());
				}
			}
		});
		
		view.getMenuItemAddToZoneDashboard().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				getRpcProvider().getDeviceService().addToZoneDashboard(
						player.getId(), player.getZoneId(), new MethodCallback<DeviceDto>() {
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
						player.getId(), player.getProjectId(), new MethodCallback<MessageDto>() {
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
							player.getId(), player.getZoneId(), new MethodCallback<MessageDto>() {
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
							player.getId(), player.getProjectId(), new MethodCallback<MessageDto>() {
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

	protected void rpcSendCommand(Command command) {
		rpcService.getPlayerService().sendCommand(player.getId(), command, new MethodCallback<Status>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}

			@Override
			public void onSuccess(Method method, Status response) {
				eventBus.fireEvent(new ShowLoadingEvent(false));
				updateStatus(response);
			}
		});
	}

	private void displayStatus() {
		view.asPanel().removeStyleName("on");
		if (player.getStatus().getTime() != null) {
			view.getLblTime().setText(player.getStatus().getTime());
		}
			
		if (Boolean.FALSE.equals(player.getStatus().getValue())) {
			view.getImgIcon().removeStyleName("on");
			view.getImgIcon().removeStyleName("unknown");
			view.getImgIcon().addStyleName("off");
		}
		else if (Boolean.TRUE.equals(player.getStatus().getValue())) {
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
		
		if (Boolean.TRUE.equals(player.getStatus().getValue())) {
			if (player.getStatus().getTrack() != null && !player.getStatus().getTrack().isEmpty()) {
				view.getLblTime().setText(player.getStatus().getTrack());
			}
			else {
				view.getLblTime().setText(player.getStatus().getTime());
			}
			if (State.playing.toString().equals(player.getStatus().getState())) {
				view.getBtnPlay().setIconType(IconType.PAUSE);
			}
			else {
				view.getBtnPlay().setIconType(IconType.PLAY_ARROW);
			}
			view.getBtnStop().setIconColor(Color.YELLOW_LIGHTEN_3);
			view.getBtnPlay().setIconColor(Color.YELLOW_LIGHTEN_3);
		}
		else {
			view.getBtnPlay().setIconType(IconType.PLAY_ARROW);
			view.getLblTime().setText(player.getStatus().getTime());
			view.getBtnStop().setIconColor(Color.GREY_LIGHTEN_3);
			view.getBtnPlay().setIconColor(Color.GREY_LIGHTEN_3);
		}
		view.getLblPlayed().setWidth(player.getStatus().getPosition() + "%");
	}

	@Override
	public void updateStatus(StatusDto status) {
		player.getStatus().setValue(status.getValue());
		player.getStatus().setState(status.getState());
		player.getStatus().setTime(status.getTime());
		player.getStatus().setTrack(status.getTrack());
		player.getStatus().setPosition(status.getPosition());
		displayStatus();
	}

	private void updateStatus(Status status) {
		player.getStatus().setValue(true);
		player.getStatus().setState(status.getState());
		player.getStatus().setTrack(status.getTitle());
		if (status.getPosition() > 0) {
			player.getStatus().setPosition(Math.round(status.getPosition() * 100));
		}
		displayStatus();
	}

	@Override
	protected PlayerView getDeviceView() {
		return view;
	}
}
