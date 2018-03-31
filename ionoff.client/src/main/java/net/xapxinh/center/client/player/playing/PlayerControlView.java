package net.xapxinh.center.client.player.playing;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.Label;

public class PlayerControlView extends FlowPanel implements PlayerControlPresenter.Display {
	
	private PlayingScreenView screenView;
	private PlayerBasicControlView basicConrolView;
	private PlayerExtControlView extConrolView;
	private PopupPlayingView popupPlayingView;
	private final Button btnExpand;

	public PlayerControlView() {
		setStyleName("playing");
		screenView = new PlayingScreenView();
		add(screenView);
		
		basicConrolView = new PlayerBasicControlView();
		add(basicConrolView);
		extConrolView = new PlayerExtControlView();
		add(extConrolView);
		popupPlayingView = new PopupPlayingView();
		
		btnExpand = new Button("");
		btnExpand.addStyleName("expand");
		add(btnExpand);
	}

	@Override
	public Button getBtnExpand() {
		return btnExpand;
	}

	@Override
	public Button getBtnPlayPause() {
		return basicConrolView.getBtnPlayPause();
	}

	@Override
	public HTML getLblTitle() {
		return screenView.getLblTitle();
	}

	@Override
	public Label getLblSeekSlider() {
		return screenView.getLblSeekSlider();
	}

	@Override
	public Button getBtnPrev() {
		return basicConrolView.getBtnPrev();
	}

	@Override
	public Button getBtnRw() {
		return extConrolView.getBtnRw();
	}

	@Override
	public Button getBtnStop() {
		return extConrolView.getBtnStop();
	}

	@Override
	public Button getBtnFw() {
		return extConrolView.getBtnFw();
	}

	@Override
	public Button getBtnNext() {
		return basicConrolView.getBtnNext();
	}

	@Override
	public Button getBtnVolMute() {
		return extConrolView.getBtnVolMute();
	}

	@Override
	public Button getBtnVolDown() {
		return extConrolView.getBtnVolDown();
	}

	@Override
	public Label getLblVolSlider() {
		return extConrolView.getLblVolSlider();
	}

	@Override
	public Button getBtnVolUp() {
		return extConrolView.getBtnVolUp();
	}

	@Override
	public Button getBtnFullScreen() {
		return extConrolView.getBtnFullScreen();
	}

	@Override
	public Label getLblTime() {
		return screenView.getLblTime();
	}

	@Override
	public Label getLblLength() {
		return screenView.getLblLength();
	}

	@Override
	public int getPlayingSliderWidth() {
		return screenView.getSeekSliderWidth();
	}

	@Override
	public int getVolSliderWidth() {
		return basicConrolView.getVolSliderWidth();
	}

	@Override
	public int getMaxTitleLength() {
		return screenView.getMaxTitleLength();
	}
	
	@Override
	public PlayingScreenView getScreenView() {
		return screenView;
	}
	
	@Override
	public PlayerBasicControlView getBasicConrolView() {
		return basicConrolView;
	}
	
	@Override
	public PopupPlayingView getPopupPlayingView() {
		return popupPlayingView;
	}
	
	@Override
	public FlowPanel asPanel() {
		return this;
	}
}
