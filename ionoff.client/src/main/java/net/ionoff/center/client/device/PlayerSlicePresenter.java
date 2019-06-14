package net.ionoff.center.client.device;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;
import gwt.material.design.client.constants.Color;
import gwt.material.design.client.constants.IconType;
import net.ionoff.center.client.event.ChangeTokenEvent;
import net.ionoff.center.client.mediaplayer.event.ShowLoadingEvent;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.MediaPlayerDto;
import net.ionoff.center.shared.dto.player.Command;
import net.ionoff.center.shared.dto.player.PlayerApi;
import net.ionoff.center.shared.dto.player.StateDto;
import net.ionoff.center.shared.dto.player.StatusDto;
import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

public class PlayerSlicePresenter extends DeviceSlicePresenter {

	private MediaPlayerDto player;
	private final PlayerSliceView view;
	private final IRpcServiceProvider rpcService;

	public PlayerSlicePresenter(IRpcServiceProvider rpcService, HandlerManager eventBus, PlayerSliceView view, MediaPlayerDto player) {
		super(rpcService, eventBus, player);
		this.view = view;
		this.player = player;
		this.rpcService = rpcService;
	}

	@Override
	public void show(HasWidgets container) {
		container.add(view.asPanel());
	}

	@Override
	public void bind() {
		view.getLblTime().setVisible(false);
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
        view.getBtnIcon().addClickHandler(new ClickHandler() {
            @Override
            public void onClick(ClickEvent event) {
                if (StateDto.playing.toString().equals(player.getStatus().getState())) {
                    rpcSendCommand(PlayerApi.pausePlaylistLeaf());
                }
                else if (StateDto.paused.toString().equals(player.getStatus().getState())) {
                    rpcSendCommand(PlayerApi.resumePlaylistLeaf());
                }
                else {
                    rpcSendCommand(PlayerApi.playPlaylist());
                }
            }
        });
		view.getBtnStop().addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					rpcSendCommand(PlayerApi.stopPlaylist());
				}
			});
		view.getBtnNext().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
                rpcSendCommand(PlayerApi.nextPlaylistLeaf());
			}
		});

	}

	protected void rpcSendCommand(Command command) {
		rpcService.getPlayerService().sendCommand(player.getId(), command, new MethodCallback<StatusDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}

			@Override
			public void onSuccess(Method method, StatusDto response) {
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
		}
		else if (Boolean.TRUE.equals(player.getStatus().getValue())) {
			view.asPanel().addStyleName("on");
		}
		else {}

		if (Boolean.TRUE.equals(player.getStatus().getValue())) {
			if (player.getStatus().getTrack() != null && !player.getStatus().getTrack().isEmpty()) {
				view.getLblTime().setText(player.getStatus().getTrack());
			}
			else {
				view.getLblTime().setText(player.getStatus().getTime());
			}
			if (StateDto.playing.toString().equals(player.getStatus().getState())) {
				view.getBtnIcon().setIconType(IconType.PAUSE_CIRCLE_FILLED);
			}
			else {
				view.getBtnIcon().setIconType(IconType.PLAY_CIRCLE_FILLED);
			}
			view.getBtnStop().setIconColor(Color.YELLOW_LIGHTEN_3);
			view.getBtnNext().setIconColor(Color.YELLOW_LIGHTEN_3);
		}
		else {
			view.getBtnIcon().setIconType(IconType.PLAY_CIRCLE_FILLED);
			view.getLblTime().setText(player.getStatus().getTime());
			view.getBtnStop().setIconColor(Color.GREY_LIGHTEN_3);
			view.getBtnNext().setIconColor(Color.GREY_LIGHTEN_3);
		}
		view.getLblPlayed().setWidth(player.getStatus().getPosition() + "%");
	}

	@Override
	public void updateStatus(net.ionoff.center.shared.dto.StatusDto status) {
		player.getStatus().setValue(status.getValue());
		player.getStatus().setState(status.getState());
		player.getStatus().setTime(status.getTime());
		player.getStatus().setTrack(status.getTrack());
		player.getStatus().setPosition(status.getPosition());
		displayStatus();
	}

	private void updateStatus(StatusDto status) {
		player.getStatus().setValue(true);
		player.getStatus().setState(status.getState());
		player.getStatus().setTrack(status.getTitle());
		if (status.getPosition() > 0) {
			player.getStatus().setPosition(Math.round(status.getPosition() * 100));
		}
		displayStatus();
	}

	@Override
	protected PlayerSliceView getDeviceView() {
		return view;
	}
}
