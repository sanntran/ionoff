package net.xapxinh.center.client.player.playing;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.PopupPanel;

public class PopupPlayingView extends PopupPanel implements PopupPlayingPresenter.Display {
	
	private FlowPanel container;
	private PlayingListView playlistView;
	private PlayerExtControlView extConrolView;
	private Button btnClose;
	
	public PopupPlayingView() {
		super(false, false);
		setStyleName("popup playing");
		setAnimationEnabled(true);
		setAnimationType(AnimationType.ROLL_DOWN);
		getElement().setId(getId());
		
		container = new FlowPanel();
		playlistView = new PlayingListView();
		extConrolView = new PlayerExtControlView();
		
		container.add(playlistView);
		container.add(extConrolView);
		
		btnClose = new Button("");
		btnClose.addStyleName("close");
		container.add(btnClose);
		
		setWidget(container);
	}
	
	@Override
	public PlayingListView getPlaylistView() {
		return playlistView;
	}
	
	@Override
	public PlayerExtControlView getExtControlView() {
		return extConrolView;
	}
	
	public FlowPanel getContainer() {
		return container;
	}
	
	@Override
	public String getId() {
		return "popup.playlist";
	}
	@Override
	public PopupPanel asPopup() {
		return this;
	}

	@Override
	public Button getBtnClose() {
		return this.btnClose;
	}
}
