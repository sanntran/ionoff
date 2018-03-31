package net.xapxinh.center.client.player.playing;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.Image;
import com.google.gwt.user.client.ui.Label;

public class PlayingScreenView extends FlowPanel {
	
	private final Image imgAlbum;
	private final Label lblAlbumName;
	private final FlowPanel playingSliderPanel;
	private final Label lblPlayingSlider;
	private final Label lblTime;
	private final Label lblLength;
	private final HTML lblFileName;
	
	public PlayingScreenView() {

		setStyleName("screen");
		
		FlowPanel fPanel2 = new FlowPanel();
		add(fPanel2);
		
		lblTime = new Label("");
		fPanel2.add(lblTime);
		lblTime.setStyleName("time");
		
		playingSliderPanel = new FlowPanel();
		fPanel2.add(playingSliderPanel);
		playingSliderPanel.setStyleName("seekSlider");
		
		lblPlayingSlider = new Label("");
		lblPlayingSlider.setStyleName("seekSliderStatus");
		playingSliderPanel.add(lblPlayingSlider);
		lblPlayingSlider.setSize("4px", "2px");
		
		lblLength = new Label("");
		fPanel2.add(lblLength);
		lblLength.setStyleName("length");
		
		FlowPanel albumImgWrapper = new FlowPanel();
		albumImgWrapper.setStyleName("albumImgWrapper");
		
		add(albumImgWrapper);
		imgAlbum = new Image();
		imgAlbum.setUrl("styles/images/play/cd.png");
		albumImgWrapper.add(imgAlbum);
		imgAlbum.setStyleName("albumImg");
		
		FlowPanel wrapper = new FlowPanel();
		wrapper.setStyleName("wrapper");
		add(wrapper);
		
		lblAlbumName = new Label("");
		wrapper.add(lblAlbumName);
		lblAlbumName.setStyleName("albumName");
		
		FlowPanel titleWrapper = new FlowPanel();
		titleWrapper.setStyleName("titleWrapper");
		wrapper.add(titleWrapper);
		
		lblFileName = new HTML("");
		lblFileName.setStyleName("title");
		titleWrapper.add(lblFileName);
	}
	
	public int getSeekSliderWidth() {
		return playingSliderPanel.getOffsetWidth();
	}

	public FlowPanel asPanel() {
		return this;
	}
	
	public Image getImgAlbum() {
		return this.imgAlbum;
	}
	
	public Label getLblAlbumName() {
		return this.lblAlbumName;
	}
	
	public Label getLblSeekSlider() {
		return this.lblPlayingSlider;
	}

	public Label getLblTime() {
		return this.lblTime;
	}

	
	public Label getLblLength() {
		return this.lblLength;
	}

	public HTML getLblTitle() {
		return this.lblFileName;
	}

	public int getMaxTitleLength() {
		return 260;
	}
}
