package net.xapxinh.center.client.player.playing;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Label;

public class PlayerExtControlView extends FlowPanel {
	
	private final Button btnReward;
	private final Button btnFoward;
	private final Button btnStop;
	private final Button btnVolMute;
	private final Button btnFullscreen;
	private final Button btnVolDown;
	private final Label lblVolSlider;
	private final Button btnVolUp;

	public PlayerExtControlView() {
		
		setStyleName("extControl");
		
		btnReward = new Button("");
		btnReward.addStyleName("rw");
		add(btnReward);

		btnStop = new Button("");
		add(btnStop);
		btnStop.addStyleName("stop");

		btnFoward = new Button("");
		btnFoward.addStyleName("fw");
		add(btnFoward);
				
		btnVolMute = new Button("");
		//add(btnVolMute);
		btnVolMute.addStyleName("volMute");
		
		btnVolDown = new Button("");
		add(btnVolDown);
		btnVolDown.addStyleName("volDown");
		
		FlowPanel volSliderPanel = new FlowPanel();
		add(volSliderPanel);
		volSliderPanel.setStyleName("volSlider");
		
		lblVolSlider = new Label("");
		volSliderPanel.add(lblVolSlider);
		lblVolSlider.setStyleName("volSliderStatus");
		lblVolSlider.setWidth("12px");
		
		btnVolUp = new Button("");
		add(btnVolUp);
		btnVolUp.addStyleName("volUp");

		btnFullscreen = new Button("");
		add(btnFullscreen);
		btnFullscreen.addStyleName("fullScreen");
	}

	public FlowPanel asPanel() {
		return this;
	}
	
	public Button getBtnRw() {
		return this.btnReward;
	}
	
	public Button getBtnFw() {
		return this.btnFoward;
	}
	
	public Button getBtnStop() {
		return this.btnStop;
	}

	public Button getBtnVolMute() {
		return this.btnVolMute;
	}
	
	public Button getBtnFullScreen() {
		return this.btnFullscreen;
	}
	

	public Button getBtnVolDown() {
		return this.btnVolDown;
	}

	public Label getLblVolSlider() {
		return this.lblVolSlider;
	}

	public Button getBtnVolUp() {
		return this.btnVolUp;
	}
}
