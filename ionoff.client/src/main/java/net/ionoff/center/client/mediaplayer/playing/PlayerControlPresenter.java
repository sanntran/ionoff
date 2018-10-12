package net.ionoff.center.client.mediaplayer.playing;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Label;

import net.ionoff.center.client.mediaplayer.AbstractPresenter;
import net.ionoff.center.client.mediaplayer.PlayerPresenter;
import net.ionoff.center.client.mediaplayer.PlayerUtil;
import net.ionoff.center.shared.dto.player.PlayListDto;
import net.ionoff.center.shared.dto.player.PlayerApi;
import net.ionoff.center.shared.dto.player.StateDto;
import net.ionoff.center.shared.dto.player.StatusDto;

/**
 * @author Sann Tran
 */
public class PlayerControlPresenter extends AbstractPresenter {

	public interface Display {

		FlowPanel asPanel();

		Button getBtnPlayPause();

		HTML getLblTitle();

		Label getLblSeekSlider();

		Button getBtnPrev();

		Button getBtnRw();

		Button getBtnStop();

		Button getBtnFw();

		Button getBtnNext();

		Button getBtnVolMute();

		Button getBtnVolDown();

		Label getLblVolSlider();

		Button getBtnVolUp();

		Button getBtnFullScreen();

		Label getLblTime();

		Label getLblLength();

		int getPlayingSliderWidth();

		int getVolSliderWidth();
		
		int getMaxTitleLength();

		PlayingScreenView getScreenView();

		PlayerBasicControlView getBasicConrolView();

		Button getBtnExpand();

		PopupPlayingView getPopupPlayingView();
	}
	
	protected final Display display;
	protected Timer playingTimer;
	protected String prevTitle = null;
	protected StatusDto currentStatus = null;
	protected long playingTime = 0;
	private final PlayerPresenter playerPresenter;
	private PopupPlayingPresenter popupPlayingPresenter;
	
	public PlayerControlPresenter(HandlerManager eventBus, Display view, 
			PlayerPresenter playerPresenter) {
		
		super(eventBus);
		this.display = view;
		this.playerPresenter = playerPresenter;
	}
	
	public void bind() {
		display.getBtnPlayPause().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (isPlaying()) {
					playerPresenter.rpcSendCommand(PlayerApi.pausePlaylistLeaf());
				}
				else if (isStopped()) {
					playerPresenter.rpcSendCommand(PlayerApi.playPlaylist());
				}
				else if (isPaused()) {
					playerPresenter.rpcSendCommand(PlayerApi.resumePlaylistLeaf());
				}
			}
		});
		display.getBtnPrev().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (currentStatus == null) {
					return;
				}
				playerPresenter.rpcSendCommand(PlayerApi.prevPlaylistLeaf());
			}
		});
		display.getBtnRw().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				getPopupPlayingPresenter().onRw();
			}
		});
		display.getBtnStop().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				getPopupPlayingPresenter().onStop();
			}
		});
		display.getBtnFw().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				getPopupPlayingPresenter().onFw();
			}
		});
		
		display.getBtnNext().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (currentStatus == null) {
					return;
				}
				playerPresenter.rpcSendCommand(PlayerApi.nextPlaylistLeaf());
			}
		});
		display.getBtnVolDown().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				getPopupPlayingPresenter().onVolDown();
			}
		});
		display.getBtnVolUp().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				getPopupPlayingPresenter().onVolUp();
			}
		});
		
		display.getBtnFullScreen().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				getPopupPlayingPresenter().onFullScreen();
			}
		});
		
		display.getBtnExpand().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (getPopupPlayingPresenter().isShowing()) {
					getPopupPlayingPresenter().hide();
				}
				else {
					getPopupPlayingPresenter().show();
					playerPresenter.getStatus();
				}
			}
		});
	}

	public void setStatus(StatusDto status) {
		currentStatus = status;
		getPopupPlayingPresenter().setStatus(status);
		if (currentStatus == null) {
			prevTitle = null;
			cancelTitleAnimationTimer();
			return;
		}
		if (isStopped()) {
			prevTitle = null;
			resetStatus();
			cancelTitleAnimationTimer();
			setVolSliderWidth(getVolSliderWidth());
			return;
		}
		
		prevTitle = currentStatus.getTitle();
		
		if (isPaused()) {
			display.getBtnPlayPause().setStyleName("gwt-Button play");
		}
		else if (isPlaying()) {
			display.getBtnPlayPause().setStyleName("gwt-Button pause");
		}
		if (!isStopped() && playingTimer == null) {
			startPlayingTimer();
		}
		setVolSliderWidth(getVolSliderWidth());
		long time = playingTime;
		try {
			time = currentStatus.getTime();
		}
		catch (NumberFormatException e) {
			return;
		}
		
		if (!currentStatus.getTitle().equals(prevTitle)
				|| playingTime - time>= 3
				|| time - playingTime >= 3) {
			playingTime = currentStatus.getTime();
		}				
	}
	
	protected void resetStatus() {
		if (playingTimer != null) {
			playingTimer.cancel();
		}
		display.getBtnPlayPause().setStyleName("gwt-Button play");
		setPlayingTitle("");
		display.getScreenView().getLblAlbumName().setText("");
		display.getLblLength().setText("--:--");
		display.getLblTime().setText("--:--");
		display.getLblSeekSlider().setWidth("4px");
		setVolSliderWidth(2);
	}

	private String getSeekSliderWidth() {
		int width = 0;
		if (currentStatus != null) {
			long time = currentStatus.getTime();
			long length = currentStatus.getLength();
			float rate = (float) time / length;
			width = (int) (rate * display.getPlayingSliderWidth());
		}
		if (width < 4) {
			width = 4;
		}
		return width + "px";
	}

	private int getVolSliderWidth() {
		int minWidth = 2;		
		int vol = currentStatus.getVolume();
		if (vol > PlayerApi.VOL_MAX) {
			vol = PlayerApi.VOL_MAX;
		}
		float volRate = (float) vol / PlayerApi.VOL_MAX;
		int width = (int) (volRate * display.getVolSliderWidth());
		if (width > minWidth) {
			return width;
		}
		return minWidth;
	}

	private void setVolSliderWidth(int pixel) {
		display.getLblVolSlider().setWidth(pixel + "px");
		getPopupPlayingPresenter().getLblVolSliderWidth(pixel + "px");
	}
	
	protected PopupPlayingPresenter getPopupPlayingPresenter() {
		if (popupPlayingPresenter == null) {
			popupPlayingPresenter = new PopupPlayingPresenter(playerPresenter, eventBus, display.getPopupPlayingView());
			popupPlayingPresenter.go();
		}
		return popupPlayingPresenter;
	}

	private void cancelTitleAnimationTimer() {
		if (playingTimer != null) {
			playingTimer.cancel();
			playingTimer = null;
		}
	}

	private void startPlayingTimer() {
		if (playingTimer != null) {
			return;
		}
		playingTimer = new Timer() { // this timer is to show playing
			
			@Override
			public void run() {
				setPlayingTitle(currentStatus.getTitle());
				if (isPlaying()) {
					if (playingTime > currentStatus.getLength()) {
						currentStatus.setTime(0);
						currentStatus.setLength(0);
						setPlayingTitle("");
					}
					else {
						playingTime = playingTime + 1;
					}
				}
				if (currentStatus.getPlayNode() != null && currentStatus.getPlayNode().getImage() != null) {
					display.getScreenView().getImgAlbum().setUrl(currentStatus.getPlayNode().getImage());
				}
				else {
					display.getScreenView().getImgAlbum().setUrl("styles/images/play/cd.png");
				}
				if (currentStatus.getPlayNode() != null && currentStatus.getPlayNode().getName() != null) {
					display.getScreenView().getLblAlbumName().setText(currentStatus.getPlayNode().getName());
				}
				else {
					display.getScreenView().getLblAlbumName().setText("");
				}
				
				display.getLblTime().setText(PlayerUtil.formatTimePlayer(playingTime));
				display.getLblSeekSlider().setWidth(getSeekSliderWidth());
				display.getLblLength().setText(PlayerUtil.formatTimePlayer(currentStatus.getLength()));
			}

		};
		playingTimer.scheduleRepeating(1000);
	}

	private boolean isPlaying() {
		return currentStatus != null && StateDto.playing.toString().equalsIgnoreCase(currentStatus.getState());
	}
	
	private boolean isStopped() {
		return currentStatus != null && StateDto.stopped.toString().equals(currentStatus.getState());
	}
	
	private boolean isPaused() {
		return currentStatus != null && StateDto.paused.toString().equals(currentStatus.getState());
	}
	
	private void setPlayingTitle(String title) {
		if (title == null) {
			display.getLblTitle().setText("");
			
		}
		if (!title.equals(display.getLblTitle().getText())) {
			display.getLblTitle().setText(title);
		}
	}

	@Override
	public void go() {
		bind();
	}

	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(display.asPanel());
	}

	public boolean isShowingPlaylist() {
		return getPopupPlayingPresenter().isShowing();
	}
	
	public void hidePlaylist() {
		getPopupPlayingPresenter().hide();
	}

	public void setPlaylist(PlayListDto result) {
		getPopupPlayingPresenter().showPlaylist(result);
	}
}
