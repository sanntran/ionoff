package net.xapxinh.center.client.player.playing;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.PopupPanel;
import com.google.gwt.user.client.ui.RootPanel;

import net.xapxinh.center.client.player.AbstractPresenter;
import net.xapxinh.center.client.player.PlayerPresenter;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.PlayerApi;
import net.xapxinh.center.shared.dto.Status;

public class PopupPlayingPresenter extends AbstractPresenter {

	public interface Display {
		PopupPanel asPopup();
		String getId();
		PlayingListView getPlaylistView();
		PlayerExtControlView getExtControlView();
		Button getBtnClose();
	}
	
	private Display display;
	private PlayingListPresenter playlistPresenter;
	private final PlayerPresenter playerPresenter;
	protected Status currentStatus;
	
	public PopupPlayingPresenter(PlayerPresenter playerPresenter, HandlerManager eventBus, Display view) {
		super(eventBus);
		this.playerPresenter = playerPresenter;
		this.display = view;
	}
	
	public boolean isShowing() {
		return RootPanel.get(display.getId()) != null;
	}

	@Override
	public void go() {
		bind();
	}

	private void bind() {
		display.getBtnClose().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				display.asPopup().hide();
			}
		});
		
		display.getExtControlView().getBtnRw().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onRw();
			}
		});
		
		display.getExtControlView().getBtnStop().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onStop();
			}
		});
		
		display.getExtControlView().getBtnFw().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onFw();
			}
		});
		
		display.getExtControlView().getBtnVolDown().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onVolDown();
			}
		});
		
		display.getExtControlView().getBtnVolUp().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onVolUp();
			}
		});
		
		display.getExtControlView().getBtnFullScreen().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onFullScreen();
			}
		});
	}

	void onStop() {
		if (currentStatus == null) {
			return;
		}
		playerPresenter.rpcSendCommand(PlayerApi.stopPlaylist());
	}
	
	void onRw() {
		if (currentStatus == null) {
			return;
		}
		if (!"stopped".equals(currentStatus.getState())) {
			playerPresenter.rpcSendCommand(PlayerApi.seekReward(), false);
		}
	}
	
	void onFw() {
		if (currentStatus == null) {
			return;
		}
		if (!"stopped".equals(currentStatus.getState())) {
			playerPresenter.rpcSendCommand(PlayerApi.seekForward(), false);
		}
	}
	
	void onVolDown() {
		if (currentStatus == null) {
			return;
		}
		playerPresenter.rpcSendCommand(PlayerApi.volumeDown(), false);
	}
	
	void onVolUp() {
		if (currentStatus == null || currentStatus.getVolume() > PlayerApi.VOL_MAX) {
			return;
		}
		playerPresenter.rpcSendCommand(PlayerApi.volumeUp(), false);
	}
	
	void onFullScreen() {
		if (currentStatus == null) {
			return;
		}
		playerPresenter.rpcSendCommand(PlayerApi.fullScreen(), false);
	}

	@Override
	public void show(HasWidgets container) {
		display.asPopup().show();
	}
	
	public void show() {
		display.asPopup().show();
	}
	
	protected PlayingListPresenter getPlaylistInstance() {
		if (playlistPresenter == null) {
			playlistPresenter = new PlayingListPresenter(eventBus, display.getPlaylistView(), playerPresenter);
			playlistPresenter.go();
		}
		return playlistPresenter;
	}

	public void getLblVolSliderWidth(String width) {
		display.getExtControlView().getLblVolSlider().setWidth(width);
	}

	public void setStatus(Status status) {
		this.currentStatus = status;
		getPlaylistInstance().setStatus(status);
	}

	public void showPlaylist(PlayListDto playlist) {
		getPlaylistInstance().showPlaylist(playlist);
	}

	public void hide() {
		display.asPopup().hide();
	}
}
