package net.xapxinh.center.client.player.playing;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;

public class PlayerBasicControlView extends FlowPanel {
	
	private final Button btnPlayPause;
	private final Button btnPrev;
	private final Button btnNext;

	public PlayerBasicControlView() {
		
		setStyleName("basicControl");
		
		btnPrev = new Button("");
		add(btnPrev);
		btnPrev.addStyleName("prev");

		btnPlayPause = new Button("");
		btnPlayPause.addStyleName("play");
		add(btnPlayPause);
				
		btnNext = new Button("");
		add(btnNext);
		btnNext.addStyleName("next");
	}

	public FlowPanel asPanel() {
		return this;
	}

	public Button getBtnPlayPause() {
		return this.btnPlayPause;
	}

	public Button getBtnPrev() {
		return this.btnPrev;
	}

	public Button getBtnNext() {
		return this.btnNext;
	}

	public int getVolSliderWidth() {
		return 70; // = 70 - 12;
	}
}
